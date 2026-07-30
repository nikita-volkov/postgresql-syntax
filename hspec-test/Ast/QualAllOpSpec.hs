module Ast.QualAllOpSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.QualAllOp
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @QualAllOp
  itSatisfiesArbitrary @QualAllOp
