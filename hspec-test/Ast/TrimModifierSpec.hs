module Ast.TrimModifierSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.TrimModifier
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  itSatisfiesIsAst @TrimModifier
  itSatisfiesArbitrary @TrimModifier
