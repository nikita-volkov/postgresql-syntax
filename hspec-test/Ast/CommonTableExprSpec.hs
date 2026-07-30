module Ast.CommonTableExprSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.CommonTableExpr
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  itSatisfiesIsAst @CommonTableExpr
  itSatisfiesArbitrary @CommonTableExpr
