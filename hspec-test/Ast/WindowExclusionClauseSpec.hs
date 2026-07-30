module Ast.WindowExclusionClauseSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.WindowExclusionClause
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @WindowExclusionClause
  itSatisfiesArbitrary @WindowExclusionClause
