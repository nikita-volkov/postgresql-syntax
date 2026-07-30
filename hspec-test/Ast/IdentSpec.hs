module Ast.IdentSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.Ident
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @Ident
  itSatisfiesArbitrary @Ident
