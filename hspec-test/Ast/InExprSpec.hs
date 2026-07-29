module Ast.InExprSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.InExpr
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @InExpr
  itSatisfiesArbitrary @InExpr
