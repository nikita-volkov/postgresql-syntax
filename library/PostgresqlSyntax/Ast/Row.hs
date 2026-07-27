module PostgresqlSyntax.Ast.Row where

import PostgresqlSyntax.Ast.ExplicitRow
import PostgresqlSyntax.Ast.ImplicitRow
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude
import Test.QuickCheck (scale)

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

instance Arbitrary Row where
  arbitrary =
    oneof
      [ ExplicitRowRow <$> scale (`div` 2) arbitrary,
        ImplicitRowRow <$> scale (`div` 2) arbitrary
      ]
