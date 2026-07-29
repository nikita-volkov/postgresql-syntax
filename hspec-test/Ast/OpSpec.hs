module Ast.OpSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.Op
import Prelude hiding (Op)
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @Op
  itSatisfiesArbitrary @Op
