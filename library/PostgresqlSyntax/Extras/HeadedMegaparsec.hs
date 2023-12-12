{-# OPTIONS_GHC -Wno-redundant-constraints -Wno-dodgy-imports -Wno-unused-imports #-}

-- |
-- Generic helpers for HeadedMegaparsec.
module PostgresqlSyntax.Extras.HeadedMegaparsec where

import Control.Applicative.Combinators hiding (some)
import Control.Applicative.Combinators.NonEmpty
import qualified Data.Text as Text
import HeadedMegaparsec hiding (string)
import PostgresqlSyntax.Prelude hiding (bit, expr, filter, head, many, option, some, sortBy, tail, try)
import Text.Megaparsec (Parsec, Stream, TraversableStream, VisualStream)
import qualified Text.Megaparsec as Megaparsec
import qualified Text.Megaparsec.Char as MegaparsecChar
import qualified Text.Megaparsec.Char.Lexer as MegaparsecLexer

-- $setup
-- >>> testParser parser = either putStr print . run parser

-- * Executors

run :: (Ord err, VisualStream strm, TraversableStream strm, Megaparsec.ShowErrorComponent err) => HeadedParsec err strm a -> strm -> Either String a
run p = first Megaparsec.errorBundlePretty . Megaparsec.runParser (toParsec p <* Megaparsec.eof) ""

-- * Primitives

-- |
-- Lifted megaparsec\'s `Megaparsec.eof`.
eof :: (Ord err, Stream strm) => HeadedParsec err strm ()
eof = parse Megaparsec.eof

-- |
-- Lifted megaparsec\'s `Megaparsec.space`.
space :: (Ord err, Stream strm, Megaparsec.Token strm ~ Char) => HeadedParsec err strm ()
space = parse MegaparsecChar.space

-- |
-- Lifted megaparsec\'s `Megaparsec.space1`.
space1 :: (Ord err, Stream strm, Megaparsec.Token strm ~ Char) => HeadedParsec err strm ()
space1 = parse MegaparsecChar.space1

-- |
-- Lifted megaparsec\'s `Megaparsec.char`.
char :: (Ord err, Stream strm, Megaparsec.Token strm ~ Char) => Char -> HeadedParsec err strm Char
char a = parse (MegaparsecChar.char a)

-- |
-- Lifted megaparsec\'s `Megaparsec.char'`.
char' :: (Ord err, Stream strm, Megaparsec.Token strm ~ Char) => Char -> HeadedParsec err strm Char
char' a = parse (MegaparsecChar.char' a)

-- |
-- Lifted megaparsec\'s `Megaparsec.string`.
string :: (Ord err, Stream strm) => Megaparsec.Tokens strm -> HeadedParsec err strm (Megaparsec.Tokens strm)
string = parse . MegaparsecChar.string

-- |
-- Lifted megaparsec\'s `Megaparsec.string'`.
string' :: (Ord err, Stream strm, FoldCase (Megaparsec.Tokens strm)) => Megaparsec.Tokens strm -> HeadedParsec err strm (Megaparsec.Tokens strm)
string' = parse . MegaparsecChar.string'

-- |
-- Lifted megaparsec\'s `Megaparsec.takeWhileP`.
takeWhileP :: (Ord err, Stream strm) => Maybe String -> (Megaparsec.Token strm -> Bool) -> HeadedParsec err strm (Megaparsec.Tokens strm)
takeWhileP label predicate = parse (Megaparsec.takeWhileP label predicate)

-- |
-- Lifted megaparsec\'s `Megaparsec.takeWhile1P`.
takeWhile1P :: (Ord err, Stream strm) => Maybe String -> (Megaparsec.Token strm -> Bool) -> HeadedParsec err strm (Megaparsec.Tokens strm)
takeWhile1P label predicate = parse (Megaparsec.takeWhile1P label predicate)

satisfy :: (Ord err, Stream strm) => (Megaparsec.Token strm -> Bool) -> HeadedParsec err strm (Megaparsec.Token strm)
satisfy = parse . Megaparsec.satisfy

decimal :: (Ord err, Stream strm, Megaparsec.Token strm ~ Char, Integral decimal) => HeadedParsec err strm decimal
decimal = parse MegaparsecLexer.decimal

float :: (Ord err, Stream strm, Megaparsec.Token strm ~ Char, RealFloat float) => HeadedParsec err strm float
float = parse MegaparsecLexer.float

-- * Combinators

sep1 :: (Ord err, Stream strm, Megaparsec.Token strm ~ Char) => HeadedParsec err strm separtor -> HeadedParsec err strm a -> HeadedParsec err strm (NonEmpty a)
sep1 separator parser = do
  head <- parser
  endHead
  tail <- many $ separator *> parser
  return (head :| tail)

sepEnd1 :: (Ord err, Stream strm, Megaparsec.Token strm ~ Char) => HeadedParsec err strm separator -> HeadedParsec err strm end -> HeadedParsec err strm el -> HeadedParsec err strm (NonEmpty el, end)
sepEnd1 sepP endP elP = do
  headEl <- elP
  let loop !list = do
        sepP
        asum
          [ do
              end <- endP
              return (headEl :| reverse list, end),
            do
              el <- elP
              loop (el : list)
          ]
   in loop []

notFollowedBy :: (Ord err, Stream strm) => HeadedParsec err strm a -> HeadedParsec err strm ()
notFollowedBy a = parse (Megaparsec.notFollowedBy (toParsec a))
