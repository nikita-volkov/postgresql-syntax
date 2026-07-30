module Ast.WindowClauseSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.WindowClause
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @WindowClause
  itSatisfiesArbitrary @WindowClause
