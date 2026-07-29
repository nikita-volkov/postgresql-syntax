module Ast.AscDescSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.AscDesc
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @AscDesc
  itSatisfiesArbitrary @AscDesc
