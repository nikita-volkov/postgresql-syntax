module Ast.TrimModifierSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.TrimModifier
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @TrimModifier
  itSatisfiesArbitrary @TrimModifier
