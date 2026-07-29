module Ast.WhenClauseSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.WhenClause
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @WhenClause
  itSatisfiesArbitrary @WhenClause
