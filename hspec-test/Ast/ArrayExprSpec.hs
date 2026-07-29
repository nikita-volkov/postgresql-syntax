module Ast.ArrayExprSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.ArrayExpr
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @ArrayExpr
  itSatisfiesArbitrary @ArrayExpr
