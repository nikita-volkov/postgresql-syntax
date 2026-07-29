module Ast.SetClauseListSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.SetClauseList
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @SetClauseList
  itSatisfiesArbitrary @SetClauseList
