module Ast.SimpleSelectSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.SimpleSelect
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  itSatisfiesIsAst @SimpleSelect
  itSatisfiesArbitrary @SimpleSelect
