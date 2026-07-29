module Ast.WhenClauseListSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.WhenClauseList
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @WhenClauseList
  itSatisfiesArbitrary @WhenClauseList
