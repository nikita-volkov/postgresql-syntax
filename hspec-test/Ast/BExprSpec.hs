module Ast.BExprSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.BExpr
import Test.Hspec

spec :: Spec
spec = fullSpec @BExpr
