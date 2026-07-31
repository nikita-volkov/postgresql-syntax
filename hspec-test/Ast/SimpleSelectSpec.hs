module Ast.SimpleSelectSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.SimpleSelect
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @SimpleSelect
  itSatisfiesCanonicalizes @SimpleSelect
  itSatisfiesArbitrary @SimpleSelect
