module Ast.BExprIsOpSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.BExprIsOp
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  itSatisfiesIsAst @BExprIsOp
  itSatisfiesArbitrary @BExprIsOp
