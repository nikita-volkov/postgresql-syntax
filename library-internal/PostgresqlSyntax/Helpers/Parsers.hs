{-# OPTIONS_GHC -Wno-redundant-constraints -Wno-missing-signatures -Wno-dodgy-imports #-}

module PostgresqlSyntax.Helpers.Parsers
  ( module PostgresqlSyntax.Extras.HeadedMegaparsec,
    module PostgresqlSyntax.Helpers.Parsers,
  )
where

import Control.Applicative.Combinators hiding (some)
import qualified Data.HashSet as HashSet
import qualified Data.Text as Text
import HeadedMegaparsec hiding (string)
import PostgresqlSyntax.Extras.HeadedMegaparsec hiding (run)
import PostgresqlSyntax.IsAst hiding (parse, parseWithPosError, toText)
import qualified PostgresqlSyntax.KeywordSet as KeywordSet
import qualified PostgresqlSyntax.Predicate as Predicate
import PostgresqlSyntax.Prelude hiding (bit, expr, filter, fromList, head, many, option, some, sortBy, tail, try)
import PostgresqlSyntax.Settings (Settings)
import qualified Text.Megaparsec as Megaparsec
import qualified Text.Megaparsec.Char as MegaparsecChar
import qualified TextBuilder

-- $setup
-- >>> import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as HeadedMegaparsec
-- >>> testParser p = either putStr print . HeadedMegaparsec.run p

inSpace :: Parser a -> Parser a
inSpace p = space *> p <* space

commaSeparator :: Parser ()
commaSeparator = space *> char ',' *> endHead *> space

dotSeparator :: Parser ()
dotSeparator = space *> char '.' *> endHead *> space

inBrackets :: Parser a -> Parser a
inBrackets p = char '[' *> space *> p <* endHead <* space <* char ']'

inBracketsCont :: Parser a -> Parser (Parser a)
inBracketsCont p = char '[' *> endHead *> pure (space *> p <* endHead <* space <* char ']')

inParens :: Parser a -> Parser a
inParens p = char '(' *> space *> p <* endHead <* space <* char ')'

inParensCont :: Parser a -> Parser (Parser a)
inParensCont p = char '(' *> endHead *> pure (space *> p <* endHead <* space <* char ')')

inParensWithLabel :: (label -> content -> result) -> Parser label -> Parser content -> Parser result
inParensWithLabel result labelParser contentParser = do
  label <- wrapToHead labelParser
  space
  char '('
  endHead
  space
  content <- contentParser
  space
  char ')'
  pure (result label content)

inParensWithClause :: Parser clause -> Parser content -> Parser content
inParensWithClause = inParensWithLabel (const id)

trueIfPresent :: Parser a -> Parser Bool
trueIfPresent p = option False (True <$ p)

-- |
-- >>> testParser (quotedString '\'') "'abc''d'"
-- "abc'd"
quotedString :: Char -> Parser Text
quotedString q = do
  char q
  endHead
  tail <-
    parse $
      let collectChunks !bdr = do
            chunk <- Megaparsec.takeWhileP Nothing (/= q)
            let bdr' = bdr <> TextBuilder.text chunk
            Megaparsec.try (consumeEscapedQuote bdr') <|> finish bdr'
          consumeEscapedQuote bdr = do
            MegaparsecChar.char q
            MegaparsecChar.char q
            collectChunks (bdr <> TextBuilder.char q)
          finish bdr = do
            MegaparsecChar.char q
            return (TextBuilder.toText bdr)
       in collectChunks mempty
  return tail

-- |
-- >>> testParser dollarQuotedSconst "$$it's good$$"
-- "it's good"
dollarQuotedSconst :: Parser Text
dollarQuotedSconst = do
  char '$'
  quoteTag <- takeWhileP Nothing (/= '$')
  let terminator = Megaparsec.chunk $ "$" <> quoteTag <> "$"
  char '$'
  endHead
  tail <-
    parse $ do
      body <- many $ Megaparsec.notFollowedBy terminator *> Megaparsec.anySingle
      terminator
      return $ Text.pack body
  return tail

-- * Keyword-matching infrastructure

--
-- Ported from "PostgresqlSyntax.Parsing". @keywordNameFromSet@\/@keywordNameByPredicate@
-- originally built an @Ident@ directly via its @UnquotedIdent@ constructor;
-- here that constructor is taken as an explicit argument (@wrap@) so this
-- module doesn't need to depend on the (still-monolithic-for-now)
-- 'PostgresqlSyntax.Ast.Ident' type. Callers pass @UnquotedIdent@ for it.

keywordNameFromSet wrap set = keywordNameByPredicate wrap (Predicate.inSet set)

keywordNameByPredicate wrap predicate =
  fmap wrap $
    filter
      (\a -> "Reserved keyword " <> show a <> " used as an identifier. If that's what you intend, you have to wrap it in double quotes.")
      predicate
      anyKeyword

anyKeyword = parse $
  Megaparsec.label "keyword" $
    do
      firstChar <- Megaparsec.satisfy Predicate.firstIdentifierChar
      remainder <- Megaparsec.takeWhileP Nothing Predicate.notFirstIdentifierChar
      return (Text.toLower (Text.cons firstChar remainder))

-- | Expected keyword
--
-- Wraps the head in 'Megaparsec.region' to pin the reported error offset to
-- where this keyword check began. Without this, megaparsec >=9.8's stricter
-- (and correct) '(<|>)' error-merging (fix for
-- <https://github.com/mrkkrp/megaparsec/issues/412>) normalizes any error
-- whose offset lands past the enclosing alternative's start back down to
-- that start, discarding its \"expecting\" set unless the offset already
-- matches exactly. Since every failed keyword branch consumes some
-- identifier text before comparing, its offset always landed past the
-- alternative's start, so wide 'asum'/'choice' chains of 'keyword' lost
-- their combined expected-token sets under 9.8. Pinning the offset up front
-- keeps every branch's offset aligned with the alternative's start, so
-- their expected sets still union correctly.
keyword a = parse $ do
  off <- Megaparsec.getOffset
  Megaparsec.region (Megaparsec.setErrorOffset off) $ do
    firstChar <- Megaparsec.satisfy Predicate.firstIdentifierChar
    remainder <- Megaparsec.takeWhileP Nothing Predicate.notFirstIdentifierChar
    let parsedKeyword = Text.toLower (Text.cons firstChar remainder)
    if a == parsedKeyword then return parsedKeyword else empty

-- |
-- Consume a keyphrase, ignoring case and types of spaces between words.
keyphrase a =
  Text.words a
    & fmap (void . MegaparsecChar.string')
    & intersperse MegaparsecChar.space1
    & sequence_
    & (<* Megaparsec.notFollowedBy (Megaparsec.satisfy Predicate.notFirstIdentifierChar))
    & fmap (const (Text.toUpper a))
    & Megaparsec.label (show a)
    & parse
    & (<* endHead)

-- * Cross-type-family helpers

--
-- Ported from "PostgresqlSyntax.Parsing". Each of these originally called a
-- single concrete node's parser directly (@typename@, @qualOp@,
-- @symbolicExprBinOp@, @fconst@\/@iconst@); here they're generalized over
-- 'IsAst' so any node module can instantiate them for its own node type(s)
-- without this module depending on any of them.

typecastExpr :: (IsAst typename) => Settings -> a -> (a -> typename -> a) -> Parser a
typecastExpr settings prefix constr = do
  space
  string "::"
  endHead
  space
  type' <- parser settings
  return (constr prefix type')

plusedExpr expr = char '+' *> space *> expr

minusedExpr expr = char '-' *> space *> expr

qualOpExpr :: (IsAst qualOp) => Settings -> Parser b -> (qualOp -> b -> a) -> Parser a
qualOpExpr settings expr constr = constr <$> wrapToHead (parser settings) <*> (space *> expr)

symbolicBinOpExpr :: (IsAst symbolicExprBinOp) => Settings -> a -> Parser b -> (a -> symbolicExprBinOp -> b -> c) -> Parser c
symbolicBinOpExpr settings a bParser constr = do
  binOp <- label "binary operator" (space *> wrapToHead (parser settings) <* space)
  b <- bParser
  return (constr a binOp b)

iconstOrFconst :: (IsAst iconst, IsAst fconst) => Settings -> Parser (Either iconst fconst)
iconstOrFconst settings = Right <$> parser settings <|> Left <$> parser settings

-- |
-- Shared by 'PostgresqlSyntax.Ast.FuncApplicationParams' and
-- 'PostgresqlSyntax.Ast.SimpleSelect' (@ALL@\/@DISTINCT@ before a func-arg
-- list, and before a @UNION@\/@INTERSECT@\/@EXCEPT@ right-hand side,
-- respectively).
allOrDistinct :: Parser Bool
allOrDistinct = keyword "all" $> False <|> keyword "distinct" $> True

-- |
-- A ColId-like identifier parser (unreserved keyword ∪ col-name keyword)
-- restricted to exclude the given reserved words — needed wherever a
-- trailing bare word must terminate a construct instead of being consumed
-- as an identifier (e.g. 'PostgresqlSyntax.Ast.SortBy'\'s
-- @USING@\/@ASC@\/@DESC@\/@NULLS@,
-- 'PostgresqlSyntax.Ast.RelationExprOptAlias'\'s alias-terminating
-- keywords). @identParser@ is the identifier type's own plain (unfiltered)
-- parser, tried first, same as plain @ColId@ does.
filteredColIdLike :: (Text -> a) -> Parser a -> [Text] -> Parser a
filteredColIdLike wrap identParser excluded =
  label "identifier" $
    identParser
      <|> keywordNameFromSet wrap (foldr HashSet.delete (KeywordSet.unreservedKeyword <> KeywordSet.colNameKeyword) excluded)
