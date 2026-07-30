module Ast.TablesampleClauseSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.TablesampleClause
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  itSatisfiesIsAst @TablesampleClause
  itSatisfiesArbitrary @TablesampleClause
