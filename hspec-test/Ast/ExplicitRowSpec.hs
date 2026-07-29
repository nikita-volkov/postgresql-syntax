module Ast.ExplicitRowSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.ExplicitRow
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @ExplicitRow
  itSatisfiesArbitrary @ExplicitRow
