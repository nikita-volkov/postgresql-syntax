module Ast.TargetingSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.Targeting
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @Targeting
  itSatisfiesArbitrary @Targeting
