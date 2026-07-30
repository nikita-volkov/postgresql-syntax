module Ast.WhenClauseListSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.WhenClauseList
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  itSatisfiesIsAst @WhenClauseList
  itSatisfiesArbitrary @WhenClauseList
