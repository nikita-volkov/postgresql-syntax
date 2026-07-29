module Ast.OnConflictSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.OnConflict
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @OnConflict
  itSatisfiesArbitrary @OnConflict
