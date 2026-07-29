module Ast.OverClauseSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.OverClause
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @OverClause
  itSatisfiesArbitrary @OverClause
