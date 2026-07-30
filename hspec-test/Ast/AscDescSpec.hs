module Ast.AscDescSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.AscDesc
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @AscDesc
  itSatisfiesArbitrary @AscDesc
