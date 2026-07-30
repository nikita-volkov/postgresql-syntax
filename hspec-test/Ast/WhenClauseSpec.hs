module Ast.WhenClauseSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.WhenClause
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  itSatisfiesIsAst @WhenClause
  itSatisfiesArbitrary @WhenClause
