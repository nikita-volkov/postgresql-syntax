module Ast.VerbalExprBinOpSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.VerbalExprBinOp
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @VerbalExprBinOp
  itSatisfiesArbitrary @VerbalExprBinOp
