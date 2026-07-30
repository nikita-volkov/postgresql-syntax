module Ast.SetTargetListSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.SetTargetList
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @SetTargetList
  itSatisfiesArbitrary @SetTargetList
