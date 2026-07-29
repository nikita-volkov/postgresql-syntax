module Ast.ConfExprSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.ConfExpr
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @ConfExpr
  itSatisfiesArbitrary @ConfExpr
