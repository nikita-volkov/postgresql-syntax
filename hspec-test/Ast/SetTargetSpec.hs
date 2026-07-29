module Ast.SetTargetSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.SetTarget
import Test.Hspec

spec :: Spec
spec = fullSpec @SetTarget
