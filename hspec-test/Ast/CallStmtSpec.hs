module Ast.CallStmtSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.CallStmt
import Test.Hspec

spec :: Spec
spec = fullSpec @CallStmt
