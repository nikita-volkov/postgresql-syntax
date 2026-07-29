module Ast.NumericSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.Numeric
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @Numeric
  itSatisfiesArbitrary @Numeric
