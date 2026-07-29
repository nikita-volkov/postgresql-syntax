module Ast.LimitClauseSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.LimitClause
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @LimitClause
  itSatisfiesArbitrary @LimitClause
