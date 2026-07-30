module Ast.ExplicitRowSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.ExplicitRow
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @ExplicitRow
  itSatisfiesArbitrary @ExplicitRow
