module Ast.WindowDefinitionSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.WindowDefinition
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  itSatisfiesIsAst @WindowDefinition
  itSatisfiesArbitrary @WindowDefinition
