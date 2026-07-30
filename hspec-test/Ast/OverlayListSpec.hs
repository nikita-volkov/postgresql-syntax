module Ast.OverlayListSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.OverlayList
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  itSatisfiesIsAst @OverlayList
  itSatisfiesArbitrary @OverlayList
