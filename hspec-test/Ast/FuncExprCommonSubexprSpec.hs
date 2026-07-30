module Ast.FuncExprCommonSubexprSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.FuncExprCommonSubexpr
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  itSatisfiesIsAst @FuncExprCommonSubexpr
  itSatisfiesArbitrary @FuncExprCommonSubexpr
