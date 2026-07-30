module Ast.InsertColumnListSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.InsertColumnList
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  itSatisfiesIsAst @InsertColumnList
  itSatisfiesArbitrary @InsertColumnList
