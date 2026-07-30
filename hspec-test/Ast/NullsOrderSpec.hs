module Ast.NullsOrderSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.NullsOrder
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  itSatisfiesIsAst @NullsOrder
  itSatisfiesArbitrary @NullsOrder
