module Ast.BitSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.Bit
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @Bit
  itSatisfiesArbitrary @Bit
