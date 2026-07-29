module Ast.RowsfromListSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.RowsfromList
import Test.Hspec

spec :: Spec
spec = fullSpec @RowsfromList
