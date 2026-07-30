module Ast.GroupByItemSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.GroupByItem
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @GroupByItem
  itSatisfiesArbitrary @GroupByItem
