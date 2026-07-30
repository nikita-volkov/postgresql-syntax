module Ast.AnyNameSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.AnyName
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  itSatisfiesIsAst @AnyName
  itSatisfiesArbitrary @AnyName
