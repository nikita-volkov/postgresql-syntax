module Ast.SelectClauseSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.SelectBinOp
import PostgresqlSyntax.Ast.SelectClause
import PostgresqlSyntax.Ast.SimpleSelect
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @SelectClause
  itSatisfiesLeftRecursion @SelectClause @SimpleSelect @(SelectBinOp, Maybe Bool, SelectClause)
  itSatisfiesArbitrary @SelectClause
