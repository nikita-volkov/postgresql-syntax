module Ast.OpSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.Op
import Test.Hspec

spec :: Spec
spec = fullSpec @Op
