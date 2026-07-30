module Ast.QualOpSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.QualOp
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @QualOp
  itSatisfiesArbitrary @QualOp
