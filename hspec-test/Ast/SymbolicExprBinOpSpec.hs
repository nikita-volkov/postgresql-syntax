module Ast.SymbolicExprBinOpSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.SymbolicExprBinOp
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  itSatisfiesIsAst @SymbolicExprBinOp
  itSatisfiesArbitrary @SymbolicExprBinOp
