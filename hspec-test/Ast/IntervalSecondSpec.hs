module Ast.IntervalSecondSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.IntervalSecond
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @IntervalSecond
  itSatisfiesArbitrary @IntervalSecond
