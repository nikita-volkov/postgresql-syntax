module Ast.FuncExprWindowlessSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.FuncExprWindowless
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @FuncExprWindowless
  itSatisfiesArbitrary @FuncExprWindowless
