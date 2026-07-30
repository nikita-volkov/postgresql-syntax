module Ast.RowsfromItemSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.RowsfromItem
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  itSatisfiesIsAst @RowsfromItem
  itSatisfiesArbitrary @RowsfromItem
