module Ast.InsertStmtSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.InsertStmt
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @InsertStmt
  itSatisfiesArbitrary @InsertStmt
  -- https://github.com/nikita-volkov/postgresql-syntax/issues/35
  itParses @InsertStmt "insert into ta.xa (values (default))"
