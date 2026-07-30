module Ast.FuncExprWindowlessSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.FuncExprWindowless
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  itSatisfiesIsAst @FuncExprWindowless
  itSatisfiesArbitrary @FuncExprWindowless
