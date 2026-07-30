module Ast.ValuesClauseSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.ValuesClause
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @ValuesClause
  itSatisfiesArbitrary @ValuesClause
