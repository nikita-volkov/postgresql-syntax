module Ast.OffsetClauseSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.OffsetClause
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @OffsetClause
  itSatisfiesArbitrary @OffsetClause
