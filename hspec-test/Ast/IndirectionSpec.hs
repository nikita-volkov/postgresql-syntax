module Ast.IndirectionSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.Indirection
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  itSatisfiesIsAst @Indirection
  itSatisfiesArbitrary @Indirection
