module Ast.IdentSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.Ident
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  itSatisfiesIsAst @Ident
  itSatisfiesArbitrary @Ident
