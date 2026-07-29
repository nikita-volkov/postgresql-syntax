module Ast.TargetElSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.TargetEl
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @TargetEl
  itSatisfiesArbitrary @TargetEl
