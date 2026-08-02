module Ast.SelectClauseSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.SelectClause
import PostgresqlSyntax.Ast.SimpleSelect
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @SelectClause
  itSatisfiesExtends @SelectClause @SimpleSelect
  itSatisfiesArbitrary @SelectClause
