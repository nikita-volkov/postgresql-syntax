module Ast.SetClauseListSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.SetClauseList
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  itSatisfiesIsAst @SetClauseList
  itSatisfiesArbitrary @SetClauseList
