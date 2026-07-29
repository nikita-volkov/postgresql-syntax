module Ast.RowsfromItemSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.RowsfromItem
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @RowsfromItem
  itSatisfiesArbitrary @RowsfromItem
