module Ast.TimezoneSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.Timezone
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @Timezone
  itSatisfiesArbitrary @Timezone
