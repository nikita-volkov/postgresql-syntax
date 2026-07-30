module Ast.FrameClauseModeSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.FrameClauseMode
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @FrameClauseMode
  itSatisfiesArbitrary @FrameClauseMode
