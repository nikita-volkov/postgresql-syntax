module Ast.FuncExprCommonSubexprSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.FuncExprCommonSubexpr
import Test.Hspec

spec :: Spec
spec = fullSpec @FuncExprCommonSubexpr
