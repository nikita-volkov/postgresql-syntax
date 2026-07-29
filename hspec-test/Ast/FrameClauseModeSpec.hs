module Ast.FrameClauseModeSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.FrameClauseMode
import Test.Hspec

spec :: Spec
spec = fullSpec @FrameClauseMode
