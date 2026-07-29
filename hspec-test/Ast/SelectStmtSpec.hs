module Ast.SelectStmtSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.SelectStmt
import Test.Hspec

spec :: Spec
spec = fullSpec @SelectStmt
