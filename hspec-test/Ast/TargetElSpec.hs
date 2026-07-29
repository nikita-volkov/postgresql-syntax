module Ast.TargetElSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.TargetEl
import Test.Hspec

spec :: Spec
spec = fullSpec @TargetEl
