module Ast.GroupByItemSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.GroupByItem
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  itSatisfiesIsAst @GroupByItem
  itSatisfiesArbitrary @GroupByItem
