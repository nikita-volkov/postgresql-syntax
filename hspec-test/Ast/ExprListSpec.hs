module Ast.ExprListSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.ExprList
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  itSatisfiesIsAst @ExprList
  itSatisfiesArbitrary @ExprList
