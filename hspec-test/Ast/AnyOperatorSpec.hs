module Ast.AnyOperatorSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.AnyOperator
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @AnyOperator
  itSatisfiesArbitrary @AnyOperator
