module PostgresqlSyntax.Ast.SelectClause where

import PostgresqlSyntax.Algebra
import {-# SOURCE #-} PostgresqlSyntax.Ast.SelectWithParens (SelectWithParens)
import {-# SOURCE #-} PostgresqlSyntax.Ast.SimpleSelect (SimpleSelect)
import qualified PostgresqlSyntax.Helpers.Gens as Gens
import PostgresqlSyntax.Prelude
import qualified Test.QuickCheck as Qc

-- |
-- ==== References
-- @
-- select_clause:
--   |  simple_select
--   |  select_with_parens
-- @
--
-- This type's own 'IsAst' instance is a plain, non-recursive-suffix-aware
-- dispatch — the real @UNION@\/@INTERSECT@\/@EXCEPT@-chaining grammar
-- (where a @select_clause@ extends into a bigger
-- 'PostgresqlSyntax.Ast.SimpleSelect' via its @BinSimpleSelect@
-- constructor) is hosted in "PostgresqlSyntax.Ast.SimpleSelect" instead,
-- since only that module can construct @BinSimpleSelect@ values. See its
-- module documentation.
data SelectClause
  = SimpleSelectSelectClause SimpleSelect
  | WithParensSelectClause SelectWithParens
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst SelectClause where
  toTextBuilder settings = \case
    SimpleSelectSelectClause a -> toTextBuilder settings a
    WithParensSelectClause a -> toTextBuilder settings a

  -- ==== Law
  --
  -- @parser = parseMaybeExtended \@SelectClause@ — see
  -- 'PostgresqlSyntax.Ast.SimpleSelect'\'s 'PostgresqlSyntax.Algebra.ExtendedBy'
  -- instance for the real @select_clause@ grammar, including
  -- @UNION@\/@INTERSECT@\/@EXCEPT@-chaining.
  parser settings = parseMaybeExtended @SelectClause settings

-- |
-- Every @select_clause@ production except the left-recursive ones (those
-- are the @UNION@\/@INTERSECT@\/@EXCEPT@ continuations, hosted by
-- "PostgresqlSyntax.Ast.SimpleSelect"\'s
-- 'PostgresqlSyntax.Algebra.ExtendedBy' instance).
instance LeftRecursive SelectClause where
  parseBase settings =
    asum
      [ WithParensSelectClause <$> parser settings,
        SimpleSelectSelectClause <$> parseBase @SimpleSelect settings
      ]

-- |
-- A 'SimpleSelect' embeds trivially into a 'SelectClause' (it's one of its
-- two alternatives), and a 'SelectClause' of that exact shape is
-- recognizable back as one. Needed so
-- "PostgresqlSyntax.Ast.SimpleSelect"\'s
-- 'PostgresqlSyntax.Algebra.ExtendedBy' instance can fold a chain of
-- @UNION@\/@INTERSECT@\/@EXCEPT@ items onto a leading 'SelectClause'.
instance Refines SimpleSelect SelectClause where
  embed = SimpleSelectSelectClause
  project = \case
    SimpleSelectSelectClause a -> Just a
    _ -> Nothing

instance Qc.Arbitrary SelectClause where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.frequency
      [ (3, SimpleSelectSelectClause <$> Gens.downscale Qc.arbitrary),
        (1, WithParensSelectClause <$> Gens.downscale Qc.arbitrary)
      ]
