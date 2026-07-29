module Ast.JoinedTableSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.JoinedTable
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @JoinedTable
  itSatisfiesArbitrary @JoinedTable
