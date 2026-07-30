module Ast.IconstSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.Iconst
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  itSatisfiesIsAst @Iconst
  itSatisfiesArbitrary @Iconst
