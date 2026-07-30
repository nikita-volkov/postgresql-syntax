module Ast.OpSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.Op
import Test.Hspec
import Prelude hiding (Op)

spec :: Spec
spec = do
  itSatisfiesIsAst @Op
  itSatisfiesArbitrary @Op
