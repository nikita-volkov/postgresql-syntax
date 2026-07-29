module Ast.CExprSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.CExpr
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @CExpr
  itSatisfiesArbitrary @CExpr
