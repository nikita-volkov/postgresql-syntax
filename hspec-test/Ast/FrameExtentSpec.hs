module Ast.FrameExtentSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.FrameExtent
import Test.Hspec

spec :: Spec
spec = fullSpec @FrameExtent
