module Ast.ColumnrefSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.Columnref
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  itSatisfiesIsAst @Columnref
  itSatisfiesArbitrary @Columnref
