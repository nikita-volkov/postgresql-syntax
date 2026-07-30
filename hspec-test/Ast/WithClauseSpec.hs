module Ast.WithClauseSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.WithClause
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  itSatisfiesIsAst @WithClause
  itSatisfiesArbitrary @WithClause
