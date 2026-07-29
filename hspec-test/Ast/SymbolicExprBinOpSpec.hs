module Ast.SymbolicExprBinOpSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.SymbolicExprBinOp
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @SymbolicExprBinOp
  itSatisfiesArbitrary @SymbolicExprBinOp
