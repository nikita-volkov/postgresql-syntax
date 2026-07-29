module Ast.TablesampleClauseSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.TablesampleClause
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @TablesampleClause
  itSatisfiesArbitrary @TablesampleClause
