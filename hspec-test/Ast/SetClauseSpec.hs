module Ast.SetClauseSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.SetClause
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @SetClause
  itSatisfiesArbitrary @SetClause
