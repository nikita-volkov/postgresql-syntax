module Ast.ConfExprSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.ConfExpr
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @ConfExpr
  itSatisfiesArbitrary @ConfExpr
