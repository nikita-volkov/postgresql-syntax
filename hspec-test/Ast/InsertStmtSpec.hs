module Ast.InsertStmtSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.InsertStmt
import Test.Hspec

spec :: Spec
spec = fullSpec @InsertStmt
