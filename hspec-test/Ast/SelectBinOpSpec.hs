module Ast.SelectBinOpSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.SelectBinOp
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @SelectBinOp
  itSatisfiesArbitrary @SelectBinOp
