module Ast.WindowDefinitionSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.WindowDefinition
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @WindowDefinition
  itSatisfiesArbitrary @WindowDefinition
