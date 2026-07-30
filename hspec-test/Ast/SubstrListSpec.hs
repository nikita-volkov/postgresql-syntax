module Ast.SubstrListSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.SubstrList
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  itSatisfiesIsAst @SubstrList
  itSatisfiesArbitrary @SubstrList
