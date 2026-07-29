module Ast.InsertColumnItemSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.InsertColumnItem
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @InsertColumnItem
  itSatisfiesArbitrary @InsertColumnItem
