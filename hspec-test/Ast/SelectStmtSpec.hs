module Ast.SelectStmtSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.SelectStmt
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @SelectStmt
  itSatisfiesArbitrary @SelectStmt
