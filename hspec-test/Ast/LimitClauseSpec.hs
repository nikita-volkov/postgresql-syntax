module Ast.LimitClauseSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.LimitClause
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  itSatisfiesIsAst @LimitClause
  itSatisfiesArbitrary @LimitClause
