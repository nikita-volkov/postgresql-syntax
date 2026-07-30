module Ast.IndirectionSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.Indirection
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @Indirection
  itSatisfiesArbitrary @Indirection
