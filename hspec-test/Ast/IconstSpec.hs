module Ast.IconstSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.Iconst
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @Iconst
  itSatisfiesArbitrary @Iconst
