module Ast.ForLockingItemSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.ForLockingItem
import Test.Hspec

spec :: Spec
spec = fullSpec @ForLockingItem
