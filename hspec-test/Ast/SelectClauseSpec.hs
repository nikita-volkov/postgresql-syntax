module Ast.SelectClauseSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.SelectClause
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @SelectClause
  itSatisfiesLeftRecursion @SelectClause @_ @_
  itSatisfiesArbitrary @SelectClause
