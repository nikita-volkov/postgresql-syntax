module Ast.FuncExprSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.FuncExpr
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @FuncExpr
  itSatisfiesArbitrary @FuncExpr
