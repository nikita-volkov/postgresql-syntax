module Ast.AnyOperatorSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.AnyOperator
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  itSatisfiesIsAst @AnyOperator
  itSatisfiesArbitrary @AnyOperator
