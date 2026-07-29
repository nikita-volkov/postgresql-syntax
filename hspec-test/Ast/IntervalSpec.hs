module Ast.IntervalSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.Interval
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @Interval
  itSatisfiesArbitrary @Interval
