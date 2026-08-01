module Ast.DeleteStmtSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.DeleteStmt
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @DeleteStmt
  itSatisfiesArbitrary @DeleteStmt
  itParses @DeleteStmt "delete from foo as set"
  itParses @DeleteStmt "delete from foo alias where 1"
  itRejects @DeleteStmt "delete from foo set"
