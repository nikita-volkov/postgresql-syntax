module Ast.ColumnrefSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.Columnref
import Test.Hspec

spec :: Spec
spec = fullSpec @Columnref
