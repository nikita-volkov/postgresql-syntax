module Ast.OverlayListSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.OverlayList
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @OverlayList
  itSatisfiesArbitrary @OverlayList
