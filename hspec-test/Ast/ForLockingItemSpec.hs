module Ast.ForLockingItemSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.ForLockingItem
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @ForLockingItem
  itSatisfiesArbitrary @ForLockingItem
