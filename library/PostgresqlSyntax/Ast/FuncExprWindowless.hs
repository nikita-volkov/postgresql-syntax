module PostgresqlSyntax.Ast.FuncExprWindowless where

import PostgresqlSyntax.Ast.FuncApplication
import PostgresqlSyntax.Ast.FuncExprCommonSubexpr
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude
import Test.QuickCheck (scale)

-- |
-- ==== References
-- @
-- func_expr_windowless:
--   | func_application
--   | func_expr_common_subexpr
-- @
data FuncExprWindowless
  = ApplicationFuncExprWindowless FuncApplication
  | CommonSubexprFuncExprWindowless FuncExprCommonSubexpr
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst FuncExprWindowless where
  toTextBuilder = \case
    ApplicationFuncExprWindowless a -> toTextBuilder a
    CommonSubexprFuncExprWindowless a -> toTextBuilder a
  parser =
    asum
      [ CommonSubexprFuncExprWindowless <$> parser,
        ApplicationFuncExprWindowless <$> parser
      ]

instance Arbitrary FuncExprWindowless where
  arbitrary =
    oneof
      [ ApplicationFuncExprWindowless <$> scale (`div` 2) arbitrary,
        CommonSubexprFuncExprWindowless <$> scale (`div` 2) arbitrary
      ]
