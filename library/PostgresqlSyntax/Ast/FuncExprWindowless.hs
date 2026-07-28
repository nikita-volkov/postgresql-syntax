module PostgresqlSyntax.Ast.FuncExprWindowless where

import PostgresqlSyntax.Ast.FuncApplication
import PostgresqlSyntax.Ast.FuncExprCommonSubexpr
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude
import qualified Test.QuickCheck as Qc

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

instance Qc.Arbitrary FuncExprWindowless where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.oneof
      [ ApplicationFuncExprWindowless <$> Qc.scale (`div` 2) Qc.arbitrary,
        CommonSubexprFuncExprWindowless <$> Qc.scale (`div` 2) Qc.arbitrary
      ]
