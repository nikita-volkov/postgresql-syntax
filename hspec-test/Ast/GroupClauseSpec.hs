module Ast.GroupClauseSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.GroupClause
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  itSatisfiesIsAst @GroupClause
  itSatisfiesArbitrary @GroupClause
