module Ast.UpdateStmtSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.UpdateStmt
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  itSatisfiesIsAst @UpdateStmt
  itSatisfiesArbitrary @UpdateStmt
