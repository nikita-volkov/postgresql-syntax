module Ast.TargetElSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.TargetEl
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @TargetEl
  itSatisfiesArbitrary @TargetEl
