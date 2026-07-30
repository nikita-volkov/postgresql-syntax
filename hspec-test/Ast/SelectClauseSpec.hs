module Ast.SelectClauseSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.SelectClause
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  itSatisfiesIsAst @SelectClause
  itSatisfiesArbitrary @SelectClause
