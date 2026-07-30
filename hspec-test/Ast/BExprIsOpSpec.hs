module Ast.BExprIsOpSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.BExprIsOp
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @BExprIsOp
  itSatisfiesArbitrary @BExprIsOp
