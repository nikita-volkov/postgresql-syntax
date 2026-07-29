module Ast.NumericSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.Numeric
import Test.Hspec

spec :: Spec
spec = fullSpec @Numeric
