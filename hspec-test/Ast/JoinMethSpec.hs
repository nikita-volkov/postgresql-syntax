module Ast.JoinMethSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.JoinMeth
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  itSatisfiesIsAst @JoinMeth
  itSatisfiesArbitrary @JoinMeth
