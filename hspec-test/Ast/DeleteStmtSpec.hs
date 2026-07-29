module Ast.DeleteStmtSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.DeleteStmt
import Test.Hspec

spec :: Spec
spec = fullSpec @DeleteStmt
