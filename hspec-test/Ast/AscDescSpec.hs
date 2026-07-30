module Ast.AscDescSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.AscDesc
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  itSatisfiesIsAst @AscDesc
  itSatisfiesArbitrary @AscDesc
