module Ast.SelectFetchFirstValueSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.SelectFetchFirstValue
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @SelectFetchFirstValue
  itSatisfiesArbitrary @SelectFetchFirstValue
