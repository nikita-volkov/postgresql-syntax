module Ast.CallStmtSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.CallStmt
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @CallStmt
  itSatisfiesArbitrary @CallStmt
