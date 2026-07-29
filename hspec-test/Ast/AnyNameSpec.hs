module Ast.AnyNameSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.AnyName
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @AnyName
  itSatisfiesArbitrary @AnyName
