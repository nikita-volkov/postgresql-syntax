module Ast.PositionListSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.PositionList
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @PositionList
  itSatisfiesArbitrary @PositionList
