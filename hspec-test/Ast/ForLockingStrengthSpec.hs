module Ast.ForLockingStrengthSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.ForLockingStrength
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @ForLockingStrength
  itSatisfiesArbitrary @ForLockingStrength
