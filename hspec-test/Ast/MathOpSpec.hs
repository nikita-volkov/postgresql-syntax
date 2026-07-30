module Ast.MathOpSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.MathOp
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  itSatisfiesIsAst @MathOp
  itSatisfiesArbitrary @MathOp
