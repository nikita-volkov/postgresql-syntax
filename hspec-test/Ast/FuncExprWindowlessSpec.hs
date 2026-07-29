module Ast.FuncExprWindowlessSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.FuncExprWindowless
import Test.Hspec

spec :: Spec
spec = fullSpec @FuncExprWindowless
