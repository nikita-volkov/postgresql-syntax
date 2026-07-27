module PostgresqlSyntax.Ast.SelectClause where

import {-# SOURCE #-} PostgresqlSyntax.Ast.SelectWithParens (SelectWithParens)
import {-# SOURCE #-} PostgresqlSyntax.Ast.SimpleSelect (SimpleSelect)
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude
import Test.QuickCheck (scale)

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
  toTextBuilder = \case
    SimpleSelectSelectClause a -> toTextBuilder a
    WithParensSelectClause a -> toTextBuilder a
  parser =
    asum
      [ WithParensSelectClause <$> parser,
        SimpleSelectSelectClause <$> parser
      ]

instance Arbitrary SelectClause where
  arbitrary =
    oneof
      [ SimpleSelectSelectClause <$> scale (`div` 2) arbitrary,
        WithParensSelectClause <$> scale (`div` 2) arbitrary
      ]
