module Ast.ReturningClauseSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.ReturningClause
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  itSatisfiesIsAst @ReturningClause
  itSatisfiesArbitrary @ReturningClause
