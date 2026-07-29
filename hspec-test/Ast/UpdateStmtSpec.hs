module Ast.UpdateStmtSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.UpdateStmt
import Test.Hspec

spec :: Spec
spec = fullSpec @UpdateStmt
