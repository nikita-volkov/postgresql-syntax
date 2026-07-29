module Ast.FuncExprCommonSubexprSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.FuncExprCommonSubexpr
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @FuncExprCommonSubexpr
  itSatisfiesArbitrary @FuncExprCommonSubexpr
