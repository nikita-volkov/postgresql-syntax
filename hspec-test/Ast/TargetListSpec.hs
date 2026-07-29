module Ast.TargetListSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.TargetList
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @TargetList
  itSatisfiesArbitrary @TargetList
