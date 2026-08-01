module Ast.UpdateStmtSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.UpdateStmt
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @UpdateStmt
  itSatisfiesArbitrary @UpdateStmt
  itParses @UpdateStmt "update foo as set set x = 1"
  itRejects @UpdateStmt "update foo set set x = 1"
