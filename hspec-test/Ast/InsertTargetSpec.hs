module Ast.InsertTargetSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.InsertTarget
import Test.Hspec

spec :: Spec
spec = fullSpec @InsertTarget
