module Ast.ImplicitRowSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.ImplicitRow
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @ImplicitRow
  itSatisfiesArbitrary @ImplicitRow
