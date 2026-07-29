module Ast.UpdateStmtSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.UpdateStmt
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @UpdateStmt
  itSatisfiesArbitrary @UpdateStmt
