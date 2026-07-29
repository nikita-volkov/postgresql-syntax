module Ast.FrameClauseSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.FrameClause
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @FrameClause
  itSatisfiesArbitrary @FrameClause
