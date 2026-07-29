module Ast.FuncExprSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.FuncExpr
import Test.Hspec

spec :: Spec
spec = fullSpec @FuncExpr
