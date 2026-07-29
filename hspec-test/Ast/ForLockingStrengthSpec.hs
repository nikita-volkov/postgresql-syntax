module Ast.ForLockingStrengthSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.ForLockingStrength
import Test.Hspec

spec :: Spec
spec = fullSpec @ForLockingStrength
