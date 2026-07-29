module Ast.SetTargetListSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.SetTargetList
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @SetTargetList
  itSatisfiesArbitrary @SetTargetList
