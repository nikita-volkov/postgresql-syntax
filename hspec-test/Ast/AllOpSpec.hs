module Ast.AllOpSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.AllOp
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @AllOp
  itSatisfiesArbitrary @AllOp
