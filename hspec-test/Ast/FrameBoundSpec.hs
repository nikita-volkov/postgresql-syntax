module Ast.FrameBoundSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.FrameBound
import Test.Hspec

spec :: Spec
spec = fullSpec @FrameBound
