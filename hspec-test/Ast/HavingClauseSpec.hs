module Ast.HavingClauseSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.HavingClause
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @HavingClause
  itSatisfiesArbitrary @HavingClause
