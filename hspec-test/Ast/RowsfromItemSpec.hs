module Ast.RowsfromItemSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.RowsfromItem
import Test.Hspec

spec :: Spec
spec = fullSpec @RowsfromItem
