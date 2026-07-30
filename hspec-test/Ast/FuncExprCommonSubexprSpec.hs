module Ast.FuncExprCommonSubexprSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.FuncExprCommonSubexpr
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @FuncExprCommonSubexpr
  itSatisfiesArbitrary @FuncExprCommonSubexpr
