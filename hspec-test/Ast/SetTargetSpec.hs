module Ast.SetTargetSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.SetTarget
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @SetTarget
  itSatisfiesArbitrary @SetTarget
