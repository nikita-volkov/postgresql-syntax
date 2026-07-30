module Ast.CExprSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.CExpr
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  itSatisfiesIsAst @CExpr
  itSatisfiesArbitrary @CExpr
