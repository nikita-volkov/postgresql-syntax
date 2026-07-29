module Ast.ArrayBoundsSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.ArrayBounds
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @ArrayBounds
  itSatisfiesArbitrary @ArrayBounds
