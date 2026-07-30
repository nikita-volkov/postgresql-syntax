module Ast.FromListSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.FromList
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  itSatisfiesIsAst @FromList
  itSatisfiesArbitrary @FromList
