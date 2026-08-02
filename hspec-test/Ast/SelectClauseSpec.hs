module Ast.SelectClauseSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.SelectClause
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @SelectClause
  itSatisfiesExtends @SelectClause @_
  itSatisfiesArbitrary @SelectClause
