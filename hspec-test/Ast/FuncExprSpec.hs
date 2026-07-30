module Ast.FuncExprSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.FuncExpr
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  itSatisfiesIsAst @FuncExpr
  itSatisfiesArbitrary @FuncExpr
