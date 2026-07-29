module Ast.TargetingSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.Targeting
import Test.Hspec

spec :: Spec
spec = fullSpec @Targeting
