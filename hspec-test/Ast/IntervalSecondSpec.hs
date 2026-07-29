module Ast.IntervalSecondSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.IntervalSecond
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @IntervalSecond
  itSatisfiesArbitrary @IntervalSecond
