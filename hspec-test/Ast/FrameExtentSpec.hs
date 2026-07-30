module Ast.FrameExtentSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.FrameExtent
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  itSatisfiesIsAst @FrameExtent
  itSatisfiesArbitrary @FrameExtent
