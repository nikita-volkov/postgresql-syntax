module Ast.OptOrdinalitySpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.OptOrdinality
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @OptOrdinality
  itSatisfiesArbitrary @OptOrdinality
