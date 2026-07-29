module Ast.NullsOrderSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.NullsOrder
import Test.Hspec

spec :: Spec
spec = fullSpec @NullsOrder
