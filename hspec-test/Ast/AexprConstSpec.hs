module Ast.AexprConstSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.AexprConst
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @AexprConst
  itSatisfiesArbitrary @AexprConst
