module Ast.TargetListSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.TargetList
import Test.Hspec

spec :: Spec
spec = fullSpec @TargetList
