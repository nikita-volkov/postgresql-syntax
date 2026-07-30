module Ast.InsertStmtSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.InsertStmt
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @InsertStmt
  itSatisfiesArbitrary @InsertStmt
