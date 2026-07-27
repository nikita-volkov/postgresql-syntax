module PostgresqlSyntax.Ast.ArrayExpr where

import PostgresqlSyntax.Ast.ArrayExprList
import PostgresqlSyntax.Ast.ExprList
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude
import Test.QuickCheck (scale)

-- |
-- ==== References
-- @
-- array_expr:
--   | '[' expr_list ']'
--   | '[' array_expr_list ']'
--   | '[' ']'
-- @
data ArrayExpr
  = ExprListArrayExpr ExprList
  | ArrayExprListArrayExpr ArrayExprList
  | EmptyArrayExpr
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst ArrayExpr where
  toTextBuilder =
    renderInBrackets . \case
      ExprListArrayExpr a -> toTextBuilder a
      ArrayExprListArrayExpr a -> toTextBuilder a
      EmptyArrayExpr -> mempty
  parser =
    inBrackets
      $ asum
        [ ArrayExprListArrayExpr <$> parser,
          ExprListArrayExpr <$> parser,
          pure EmptyArrayExpr
        ]

instance Arbitrary ArrayExpr where
  arbitrary =
    sized $ \n ->
      if n <= 1
        then pure EmptyArrayExpr
        else
          oneof
            [ ExprListArrayExpr <$> scale (`div` 2) arbitrary,
              ArrayExprListArrayExpr <$> scale (`div` 2) arbitrary,
              pure EmptyArrayExpr
            ]
