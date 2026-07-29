module Ast.RowSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.Row
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @Row
  itSatisfiesArbitrary @Row
