module Ast.IntervalSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.Interval
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  itSatisfiesIsAst @Interval
  itSatisfiesArbitrary @Interval
