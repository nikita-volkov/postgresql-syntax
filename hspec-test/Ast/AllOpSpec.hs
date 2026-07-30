module Ast.AllOpSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.AllOp
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  itSatisfiesIsAst @AllOp
  itSatisfiesArbitrary @AllOp
