module Ast.FrameExtentSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.FrameExtent
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @FrameExtent
  itSatisfiesArbitrary @FrameExtent
