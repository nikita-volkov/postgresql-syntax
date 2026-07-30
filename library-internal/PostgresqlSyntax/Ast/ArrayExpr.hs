module PostgresqlSyntax.Ast.ArrayExpr where

import PostgresqlSyntax.Ast.ArrayExprList
import PostgresqlSyntax.Ast.ExprList
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
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
  toTextBuilder settings =
    TextBuilders.renderInBrackets . \case
      ExprListArrayExpr a -> toTextBuilder settings a
      ArrayExprListArrayExpr a -> toTextBuilder settings a
      EmptyArrayExpr -> mempty
  parser settings =
    Parsers.inBrackets $
      asum
        [ ArrayExprListArrayExpr <$> parser settings,
          ExprListArrayExpr <$> parser settings,
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
