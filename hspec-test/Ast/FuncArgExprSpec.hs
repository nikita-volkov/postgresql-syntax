module Ast.FuncArgExprSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.FuncArgExpr
import Test.Hspec

spec :: Spec
spec = fullSpec @FuncArgExpr
