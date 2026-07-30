module Ast.ExprListSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.ExprList
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @ExprList
  itSatisfiesArbitrary @ExprList
