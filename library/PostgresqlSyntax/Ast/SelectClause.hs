module PostgresqlSyntax.Ast.SelectClause where

import {-# SOURCE #-} PostgresqlSyntax.Ast.SelectWithParens (SelectWithParens)
import {-# SOURCE #-} PostgresqlSyntax.Ast.SimpleSelect (SimpleSelect)
import qualified PostgresqlSyntax.Extras.QuickCheck as Qc
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
  toTextBuilder = \case
    SimpleSelectSelectClause a -> toTextBuilder a
    WithParensSelectClause a -> toTextBuilder a
  parser =
    asum
      [ WithParensSelectClause <$> parser,
        SimpleSelectSelectClause <$> parser
      ]

instance Qc.Arbitrary SelectClause where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.oneof
      [ SimpleSelectSelectClause <$> Qc.downscale Qc.arbitrary,
        WithParensSelectClause <$> Qc.downscale Qc.arbitrary
      ]
