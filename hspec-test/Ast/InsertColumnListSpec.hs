module Ast.InsertColumnListSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.InsertColumnList
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @InsertColumnList
  itSatisfiesArbitrary @InsertColumnList
