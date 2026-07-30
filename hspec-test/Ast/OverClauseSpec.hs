module Ast.OverClauseSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.OverClause
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  itSatisfiesIsAst @OverClause
  itSatisfiesArbitrary @OverClause
