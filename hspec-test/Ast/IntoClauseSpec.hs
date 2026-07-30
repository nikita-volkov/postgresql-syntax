module Ast.IntoClauseSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.IntoClause
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  itSatisfiesIsAst @IntoClause
  itSatisfiesArbitrary @IntoClause
