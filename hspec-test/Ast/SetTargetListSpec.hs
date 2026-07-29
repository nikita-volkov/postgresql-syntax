module Ast.SetTargetListSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.SetTargetList
import Test.Hspec

spec :: Spec
spec = fullSpec @SetTargetList
