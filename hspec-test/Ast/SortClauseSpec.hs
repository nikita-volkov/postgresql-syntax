module Ast.SortClauseSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.SortClause
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @SortClause
  itSatisfiesArbitrary @SortClause
