module PostgresqlSyntax.Ast.SelectClause where

import {-# SOURCE #-} PostgresqlSyntax.Ast.SelectWithParens (SelectWithParens)
import {-# SOURCE #-} PostgresqlSyntax.Ast.SimpleSelect (SimpleSelect)
import qualified PostgresqlSyntax.Helpers.Gens as Gens
import PostgresqlSyntax.IsAst
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
  parser settings =
    SimpleSelectSelectClause
      <$> parser settings
        <|> WithParensSelectClause
      <$> parser settings

instance Qc.Arbitrary SelectClause where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.frequency
      [ (3, SimpleSelectSelectClause <$> Gens.downscale Qc.arbitrary),
        (1, WithParensSelectClause <$> Gens.downscale Qc.arbitrary)
      ]
