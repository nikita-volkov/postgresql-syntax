module Ast.ExtractListSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.ExtractList
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @ExtractList
  itSatisfiesArbitrary @ExtractList
