module Ast.InsertTargetSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.InsertTarget
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @InsertTarget
  itSatisfiesArbitrary @InsertTarget
