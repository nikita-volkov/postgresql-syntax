module Ast.BExprSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.BExpr
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @BExpr
  itSatisfiesArbitrary @BExpr
