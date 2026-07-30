module Ast.SelectBinOpSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.SelectBinOp
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  itSatisfiesIsAst @SelectBinOp
  itSatisfiesArbitrary @SelectBinOp
