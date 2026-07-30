module Ast.InsertRestSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.InsertRest
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  itSatisfiesIsAst @InsertRest
  itSatisfiesArbitrary @InsertRest
