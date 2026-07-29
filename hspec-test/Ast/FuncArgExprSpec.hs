module Ast.FuncArgExprSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.FuncArgExpr
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @FuncArgExpr
  itSatisfiesArbitrary @FuncArgExpr
