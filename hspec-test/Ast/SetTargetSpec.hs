module Ast.SetTargetSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.SetTarget
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @SetTarget
  itSatisfiesArbitrary @SetTarget
