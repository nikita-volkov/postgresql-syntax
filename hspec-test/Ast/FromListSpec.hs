module Ast.FromListSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.FromList
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @FromList
  itSatisfiesArbitrary @FromList
