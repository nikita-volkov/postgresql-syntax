module Ast.ForLockingItemSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.ForLockingItem
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  itSatisfiesIsAst @ForLockingItem
  itSatisfiesArbitrary @ForLockingItem
