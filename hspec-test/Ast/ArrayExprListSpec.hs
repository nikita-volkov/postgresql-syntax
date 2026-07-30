module Ast.ArrayExprListSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.ArrayExprList
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  itSatisfiesIsAst @ArrayExprList
  itSatisfiesArbitrary @ArrayExprList
