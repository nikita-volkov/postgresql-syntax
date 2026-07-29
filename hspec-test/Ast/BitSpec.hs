module Ast.BitSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.Bit
import Test.Hspec

spec :: Spec
spec = fullSpec @Bit
