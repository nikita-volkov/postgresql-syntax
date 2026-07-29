module Ast.SimpleSelectSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.SimpleSelect
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @SimpleSelect
  itSatisfiesArbitrary @SimpleSelect
