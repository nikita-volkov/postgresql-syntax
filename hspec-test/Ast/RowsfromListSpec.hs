module Ast.RowsfromListSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.RowsfromList
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @RowsfromList
  itSatisfiesArbitrary @RowsfromList
