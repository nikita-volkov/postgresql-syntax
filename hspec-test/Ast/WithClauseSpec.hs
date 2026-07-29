module Ast.WithClauseSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.WithClause
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @WithClause
  itSatisfiesArbitrary @WithClause
