module Ast.CommonTableExprSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.CommonTableExpr
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @CommonTableExpr
  itSatisfiesArbitrary @CommonTableExpr
