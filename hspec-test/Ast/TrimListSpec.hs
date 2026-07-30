module Ast.TrimListSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.TrimList
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  itSatisfiesIsAst @TrimList
  itSatisfiesArbitrary @TrimList
