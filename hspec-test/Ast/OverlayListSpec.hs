module Ast.OverlayListSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.OverlayList
import Test.Hspec

spec :: Spec
spec = fullSpec @OverlayList
