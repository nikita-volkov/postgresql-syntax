module Ast.InsertRestSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.InsertRest
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @InsertRest
  itSatisfiesArbitrary @InsertRest
