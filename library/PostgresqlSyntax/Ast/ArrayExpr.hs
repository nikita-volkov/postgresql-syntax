module PostgresqlSyntax.Ast.ArrayExpr where

import PostgresqlSyntax.Ast.ArrayExprList
import PostgresqlSyntax.Ast.ExprList
import PostgresqlSyntax.Helpers.Parsers
import PostgresqlSyntax.Helpers.TextBuilders
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude
import qualified Test.QuickCheck as Qc

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
    inBrackets $
      asum
        [ ArrayExprListArrayExpr <$> parser,
          ExprListArrayExpr <$> parser,
          pure EmptyArrayExpr
        ]

instance Qc.Arbitrary ArrayExpr where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.sized $ \n ->
      if n <= 1
        then pure EmptyArrayExpr
        else
          Qc.oneof
            [ ExprListArrayExpr <$> Qc.arbitrary,
              ArrayExprListArrayExpr <$> Qc.arbitrary,
              pure EmptyArrayExpr
            ]
