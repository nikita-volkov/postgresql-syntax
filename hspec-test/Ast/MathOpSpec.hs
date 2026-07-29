module Ast.MathOpSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.MathOp
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @MathOp
  itSatisfiesArbitrary @MathOp
