module Ast.ForLockingClauseSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.ForLockingClause
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @ForLockingClause
  itSatisfiesArbitrary @ForLockingClause
