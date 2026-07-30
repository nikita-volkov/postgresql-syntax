module Ast.AExprReversableOpSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.AExprReversableOp
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  itSatisfiesIsAst @AExprReversableOp
  itSatisfiesArbitrary @AExprReversableOp
