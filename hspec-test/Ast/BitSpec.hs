module Ast.BitSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.Bit
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @Bit
  itSatisfiesArbitrary @Bit
