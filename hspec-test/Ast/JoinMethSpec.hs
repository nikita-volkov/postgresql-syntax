module Ast.JoinMethSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.JoinMeth
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @JoinMeth
  itSatisfiesArbitrary @JoinMeth
