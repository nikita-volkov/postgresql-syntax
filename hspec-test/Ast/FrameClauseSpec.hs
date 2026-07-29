module Ast.FrameClauseSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.FrameClause
import Test.Hspec

spec :: Spec
spec = fullSpec @FrameClause
