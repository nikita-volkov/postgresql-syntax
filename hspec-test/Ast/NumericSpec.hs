module Ast.NumericSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.Numeric
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  itSatisfiesIsAst @Numeric
  itSatisfiesArbitrary @Numeric
