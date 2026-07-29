module Ast.ArrayExprListSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.ArrayExprList
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @ArrayExprList
  itSatisfiesArbitrary @ArrayExprList
