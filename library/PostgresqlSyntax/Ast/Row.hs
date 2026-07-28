module PostgresqlSyntax.Ast.Row where

import PostgresqlSyntax.Ast.ExplicitRow
import PostgresqlSyntax.Ast.ImplicitRow
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude
import qualified Test.QuickCheck as Qc

-- |
-- ==== References
-- @
-- row:
--   | ROW '(' expr_list ')'
--   | ROW '(' ')'
--   | '(' expr_list ',' a_expr ')'
-- @
data Row
  = ExplicitRowRow ExplicitRow
  | ImplicitRowRow ImplicitRow
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst Row where
  toTextBuilder = \case
    ExplicitRowRow a -> toTextBuilder a
    ImplicitRowRow a -> toTextBuilder a
  parser = ExplicitRowRow <$> parser <|> ImplicitRowRow <$> parser

instance Qc.Arbitrary Row where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.oneof
      [ ExplicitRowRow <$> Qc.scale (`div` 2) Qc.arbitrary,
        ImplicitRowRow <$> Qc.scale (`div` 2) Qc.arbitrary
      ]
