module Ast.CaseExprSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.CaseExpr
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @CaseExpr
  itSatisfiesArbitrary @CaseExpr
