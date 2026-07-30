module Ast.VerbalExprBinOpSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.VerbalExprBinOp
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  itSatisfiesIsAst @VerbalExprBinOp
  itSatisfiesArbitrary @VerbalExprBinOp
