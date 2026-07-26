{-# OPTIONS_GHC -Wno-missing-signatures #-}

-- |
-- Benchmark for the "keyword allocation cost in wide alternations" concern
-- raised in <https://github.com/nikita-volkov/postgresql-syntax/issues/21>.
--
-- Compares the shipped, cheap 'PostgresqlSyntax.Parsing.keyword'
-- (a bare 'empty' on mismatch, no allocation beyond the consumed text)
-- against a richer alternative that reports a proper \"expected one of ...\"
-- error via 'Text.Megaparsec.failure' (as PR #20 proposed), the way it would
-- behave sitting inside a wide 'asum'\/'choice' of keyword alternatives.
--
-- Both implementations here pin the reported error offset via
-- 'Megaparsec.setOffset', matching the fix applied to the shipped
-- 'PostgresqlSyntax.Parsing.keyword' to stay correct under megaparsec
-- >=9.8's stricter '(<|>)' error-merging. That fix is what makes the rich
-- variant produce complete "expecting ..." lists at all under current
-- megaparsec; see 'PostgresqlSyntax.Parsing.keyword''s haddock.
module Main where

import Criterion.Main
import qualified Data.Char as Char
import qualified Data.HashSet as HashSet
import qualified Data.Set as Set
import qualified Data.Text as Text
import HeadedMegaparsec (HeadedParsec, parse, toParsec)
import qualified Hedgehog.Gen as Gen
import qualified Main.Gen as SynGen
import qualified PostgresqlSyntax.KeywordSet as KeywordSet
import qualified PostgresqlSyntax.Parsing as Parsing
import qualified PostgresqlSyntax.Rendering as Rendering
import qualified Text.Megaparsec as Megaparsec
import Prelude

-- * Shared token scanner (mirrors 'PostgresqlSyntax.Parsing.anyKeyword')

firstIdentifierChar :: Char -> Bool
firstIdentifierChar x = Char.isAlpha x || x == '_' || x >= '\200' && x <= '\377'

notFirstIdentifierChar :: Char -> Bool
notFirstIdentifierChar x = Char.isAlphaNum x || x == '_' || x == '$' || x >= '\200' && x <= '\377'

anyKeywordRaw :: Megaparsec.Parsec Void Text.Text Text.Text
anyKeywordRaw =
  Megaparsec.label "keyword" $ do
    firstChar <- Megaparsec.satisfy firstIdentifierChar
    remainder <- Megaparsec.takeWhileP Nothing notFirstIdentifierChar
    return (Text.toLower (Text.cons firstChar remainder))

-- * The two candidate implementations

-- | Shipped implementation: fails with a bare, unlabeled 'empty' on mismatch.
cheapKeyword :: Text.Text -> HeadedParsec Void Text.Text Text.Text
cheapKeyword expected = parse $ do
  off <- Megaparsec.getOffset
  parsed <- anyKeywordRaw
  if expected == parsed then return parsed else Megaparsec.setOffset off *> empty

-- | PR #20-style implementation: fails with an explicit chunk expectation,
-- so failed alternatives can be unioned into a readable "expecting one of
-- ..." message. Allocates a 'NonEmpty' 'Char' and a 'Set' per failed branch.
richKeyword :: Text.Text -> HeadedParsec Void Text.Text Text.Text
richKeyword expected = parse $ do
  off <- Megaparsec.getOffset
  parsed <- anyKeywordRaw
  if expected == parsed then return parsed else Megaparsec.setOffset off *> chunkFailure expected parsed
  where
    chunkFailure expectedTxt givenTxt =
      Megaparsec.failure
        (Just (errorItemConverter givenTxt))
        (Set.fromList (pure (errorItemConverter expectedTxt)))
    errorItemConverter t = Megaparsec.Tokens $ case Text.uncons t of
      Just (h, rest) -> h :| Text.unpack rest
      Nothing -> ' ' :| []

runHeaded :: HeadedParsec Void Text.Text a -> Text.Text -> Either (Megaparsec.ParseErrorBundle Text.Text Void) a
runHeaded p = Megaparsec.runParser (toParsec p <* Megaparsec.eof) ""

-- * Wide-alternation scenarios

-- | An alternation over every element of the given keyword list, dispatched
-- via the given per-keyword implementation. This is the shape 'keyword'
-- actually sits in throughout the grammar: many 'HeadedParsec' alternatives
-- tried in sequence via '(<|>)'\/'asum'.
wideAlternation :: (Text.Text -> HeadedParsec Void Text.Text Text.Text) -> [Text.Text] -> HeadedParsec Void Text.Text Text.Text
wideAlternation impl kws = asum (map impl kws)

main :: IO ()
main = do
  let reservedWords = HashSet.toList KeywordSet.reservedKeyword -- ~90 words: the size of a typical mid-size dispatch
      allWords = HashSet.toList KeywordSet.keyword -- ~440 words: the full reserved-word set, worst case
      lastOf xs = last xs
      noMatchToken = "zzzznotakeyword" :: Text.Text

  corpus <- replicateM 500 (Gen.sample (Gen.resize 15 SynGen.preparableStmt))
  let corpusTexts = map (Rendering.toText . Rendering.preparableStmt) corpus

  defaultMain
    [ bgroup
        "keyword in wide alternation"
        [ bgroup
            (label <> ", " <> scenario)
            [ bench "cheap" $ whnf (runHeaded (wideAlternation cheapKeyword kws)) token,
              bench "rich" $ whnf (runHeaded (wideAlternation richKeyword kws)) token
            ]
        | (label, kws) <- [("~90 words", reservedWords), ("~440 words", allWords)],
          (scenario, token) <-
            [ ("first match", head kws),
              ("last match", lastOf kws),
              ("no match", noMatchToken)
            ]
        ],
      bgroup
        "full grammar on generated corpus (reference, shipped keyword only)"
        [bench "500 generated preparableStmt" $ nf (map (either (const False) (const True) . Parsing.run Parsing.preparableStmt)) corpusTexts]
    ]
