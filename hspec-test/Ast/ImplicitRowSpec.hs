module Ast.ImplicitRowSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.ImplicitRow
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  itSatisfiesIsAst @ImplicitRow
  itSatisfiesArbitrary @ImplicitRow
