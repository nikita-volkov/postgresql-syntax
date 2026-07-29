module Ast.AExprReversableOpSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.AExprReversableOp
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @AExprReversableOp
  itSatisfiesArbitrary @AExprReversableOp
