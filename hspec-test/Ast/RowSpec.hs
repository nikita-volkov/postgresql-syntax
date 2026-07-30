module Ast.RowSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.Row
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @Row
  itSatisfiesArbitrary @Row
