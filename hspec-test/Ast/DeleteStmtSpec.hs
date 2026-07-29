module Ast.DeleteStmtSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.DeleteStmt
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @DeleteStmt
  itSatisfiesArbitrary @DeleteStmt
