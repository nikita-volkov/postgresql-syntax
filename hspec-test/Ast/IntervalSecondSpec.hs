module Ast.IntervalSecondSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.IntervalSecond
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  itSatisfiesIsAst @IntervalSecond
  itSatisfiesArbitrary @IntervalSecond
