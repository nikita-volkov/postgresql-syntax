module PostgresqlSyntax.Ast.SelectNoParens
  ( SelectNoParens (..),
    unparenthesizedSelectNoParens,
    selectNoParensAfterClause,
    afterSelectWithParensClause,
  )
where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Ast.ForLockingClause
import PostgresqlSyntax.Ast.SelectClause
import PostgresqlSyntax.Ast.SelectLimit
import {-# SOURCE #-} PostgresqlSyntax.Ast.SelectWithParens (SelectWithParens)
import {-# SOURCE #-} qualified PostgresqlSyntax.Ast.SimpleSelect as SimpleSelect
import PostgresqlSyntax.Ast.SortClause
import {-# SOURCE #-} PostgresqlSyntax.Ast.WithClause (WithClause)
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import qualified Test.QuickCheck as Qc

-- |
-- Covers the following cases:
--
-- @
-- select_no_parens:
--   |  simple_select
--   |  select_clause sort_clause
--   |  select_clause opt_sort_clause for_locking_clause opt_select_limit
--   |  select_clause opt_sort_clause select_limit opt_for_locking_clause
--   |  with_clause select_clause
--   |  with_clause select_clause sort_clause
--   |  with_clause select_clause opt_sort_clause for_locking_clause opt_select_limit
--   |  with_clause select_clause opt_sort_clause select_limit opt_for_locking_clause
-- @
data SelectNoParens
  = SelectNoParens (Maybe WithClause) SelectClause (Maybe SortClause) (Maybe SelectLimit) (Maybe ForLockingClause)
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst SelectNoParens where
  toTextBuilder (SelectNoParens a b c d e) =
    TextBuilders.optLexemes
      [ fmap toTextBuilder a,
        Just (toTextBuilder b),
        fmap toTextBuilder c,
        fmap toTextBuilder d,
        fmap toTextBuilder e
      ]
  parser = withSelectNoParens <|> simpleSelectNoParens
    where
      simpleSelectNoParens = sharedSelectNoParens Nothing
      withSelectNoParens = do
        with <- Parser.wrapToHead parser
        Parsers.space1
        sharedSelectNoParens (Just with)

sharedSelectNoParens :: Maybe WithClause -> Parser SelectNoParens
sharedSelectNoParens with = SimpleSelect.selectClauseBase >>= selectNoParensAfterClause with

-- |
-- 'PostgresqlSyntax.Ast.SelectNoParens.parser' restricted to the forms
-- that do not begin with @(@ — see the boot-exposed signature's doc.
unparenthesizedSelectNoParens :: Parser SelectNoParens
unparenthesizedSelectNoParens =
  withSelectNoParens
    <|> (SimpleSelect.baseSimpleSelect >>= selectNoParensAfterClause Nothing . SimpleSelectSelectClause)
  where
    withSelectNoParens = do
      with <- Parser.wrapToHead parser
      Parsers.space1
      sharedSelectNoParens (Just with)

selectNoParensAfterClause :: Maybe WithClause -> SelectClause -> Parser SelectNoParens
selectNoParensAfterClause with clauseBase = do
  select <- SimpleSelect.extendSelectClause clauseBase
  sort <- optional (Parsers.space1 *> parser)
  (limit, forLocking) <- limitFirst <|> forLockingFirst <|> pure (Nothing, Nothing)
  return (SelectNoParens with select sort limit forLocking)
  where
    limitFirst = do
      limit <- Parsers.space1 *> parser
      forLocking <- optional (Parsers.space1 *> parser)
      pure (Just limit, forLocking)
    forLockingFirst = do
      forLocking <- Parsers.space1 *> parser
      limit <- optional (Parsers.space1 *> parser)
      pure (limit, Just forLocking)

-- |
-- Parses the remainder of a @select_no_parens@ whose leading clause is
-- already known to be a parenthesized select (@a@), then decides — per the
-- \"Canonical shape\" rule — whether the result collapses back down to just
-- that same parenthesized select (@Left@) or is a genuine
-- @SelectNoParens@ wrapping it (@Right@). Needed by
-- "PostgresqlSyntax.Ast.SelectWithParens", which can't pattern-match this
-- module's own 'SelectNoParens' constructor across the hub boundary.
afterSelectWithParensClause :: SelectWithParens -> Parser (Either SelectWithParens SelectNoParens)
afterSelectWithParensClause a = do
  b <- selectNoParensAfterClause Nothing (WithParensSelectClause a)
  return $ case b of
    SelectNoParens Nothing (WithParensSelectClause c) Nothing Nothing Nothing -> Left c
    _ -> Right b

instance Qc.Arbitrary SelectNoParens where
  shrink = Qc.genericShrink
  arbitrary =
    sized $ \size ->
      if size <= 1
        then do
          selectClause <- Qc.arbitrary
          return (SelectNoParens Nothing selectClause Nothing Nothing Nothing)
        else
          Qc.resize (div size 2) $
            SelectNoParens
              <$> Qc.arbitrary
              <*> Qc.arbitrary
              <*> Qc.arbitrary
              <*> Qc.arbitrary
              <*> Qc.arbitrary
