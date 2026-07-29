module Ast.ColumnrefSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.Columnref
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @Columnref
  itSatisfiesArbitrary @Columnref
