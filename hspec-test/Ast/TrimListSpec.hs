module Ast.TrimListSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.TrimList
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @TrimList
  itSatisfiesArbitrary @TrimList
