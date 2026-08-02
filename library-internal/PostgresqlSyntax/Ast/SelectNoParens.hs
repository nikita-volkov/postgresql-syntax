module PostgresqlSyntax.Ast.SelectNoParens
  ( SelectNoParens (..),
  )
where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Algebra
import PostgresqlSyntax.Ast.ForLockingClause
import PostgresqlSyntax.Ast.SelectClause
import PostgresqlSyntax.Ast.SelectLimit
import {-# SOURCE #-} PostgresqlSyntax.Ast.SelectWithParens (SelectWithParens)
import {-# SOURCE #-} PostgresqlSyntax.Ast.SimpleSelect (SimpleSelect)
import PostgresqlSyntax.Ast.SortClause
import {-# SOURCE #-} PostgresqlSyntax.Ast.WithClause (WithClause)
import qualified PostgresqlSyntax.Helpers.Gens as Gens
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
import PostgresqlSyntax.Prelude
import PostgresqlSyntax.Settings (Settings)
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
  toTextBuilder settings (SelectNoParens a b c d e) =
    TextBuilders.optLexemes
      [ fmap (toTextBuilder settings) a,
        Just (toTextBuilder settings b),
        fmap (toTextBuilder settings) c,
        fmap (toTextBuilder settings) d,
        fmap (toTextBuilder settings) e
      ]
  parser settings = withSelectNoParens <|> simpleSelectNoParens
    where
      simpleSelectNoParens = sharedSelectNoParens settings Nothing
      withSelectNoParens = do
        with <- Parser.wrapToHead (parser settings)
        Parsers.space1
        sharedSelectNoParens settings (Just with)

-- |
-- Parses the @select_clause@ (see
-- 'PostgresqlSyntax.Ast.SimpleSelect'\'s 'Extends' instance for the
-- @UNION@\/@INTERSECT@\/@EXCEPT@-chaining grammar) plus everything that can
-- follow it.
sharedSelectNoParens :: Settings -> Maybe WithClause -> Parser SelectNoParens
sharedSelectNoParens settings with = do
  select <- parseMaybeExtended @SimpleSelect settings
  sort <- optional (Parsers.space1 *> parser settings)
  (limit, forLocking) <- limitFirst <|> forLockingFirst <|> pure (Nothing, Nothing)
  return (SelectNoParens with select sort limit forLocking)
  where
    limitFirst = do
      limit <- Parsers.space1 *> parser settings
      forLocking <- optional (Parsers.space1 *> parser settings)
      pure (Just limit, forLocking)
    forLockingFirst = do
      forLocking <- Parsers.space1 *> parser settings
      limit <- optional (Parsers.space1 *> parser settings)
      pure (limit, Just forLocking)

-- |
-- If a 'SelectNoParens' is merely a trivial wrapper around a single
-- parenthesized select — no with-clause, sort, limit or locking clause of
-- its own — returns the wrapped 'SelectWithParens'. Used by the
-- 'Refines' instance to canonicalize such wrappers.
refineToSelectWithParens :: SelectNoParens -> Maybe SelectWithParens
refineToSelectWithParens = \case
  SelectNoParens Nothing (WithParensSelectClause c) Nothing Nothing Nothing -> Just c
  _ -> Nothing

instance Refines SelectWithParens SelectNoParens where
  embed c = SelectNoParens Nothing (WithParensSelectClause c) Nothing Nothing Nothing
  project = refineToSelectWithParens

instance Qc.Arbitrary SelectNoParens where
  shrink = Qc.genericShrink
  arbitrary =
    sized $ \size ->
      if size <= 1
        then do
          selectClause <- Qc.arbitrary
          return (SelectNoParens Nothing selectClause Nothing Nothing Nothing)
        else
          SelectNoParens
            <$> Gens.downscale Qc.arbitrary
            <*> Qc.arbitrary
            <*> Qc.arbitrary
            <*> Qc.arbitrary
            <*> Qc.arbitrary
