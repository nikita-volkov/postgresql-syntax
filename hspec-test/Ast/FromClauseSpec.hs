module Ast.FromClauseSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.FromClause
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @FromClause
  itSatisfiesArbitrary @FromClause
