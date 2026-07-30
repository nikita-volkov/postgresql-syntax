module Ast.ArrayBoundsSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.ArrayBounds
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  itSatisfiesIsAst @ArrayBounds
  itSatisfiesArbitrary @ArrayBounds
