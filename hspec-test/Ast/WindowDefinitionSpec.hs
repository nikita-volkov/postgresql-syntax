module Ast.WindowDefinitionSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.WindowDefinition
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @WindowDefinition
  itSatisfiesArbitrary @WindowDefinition
