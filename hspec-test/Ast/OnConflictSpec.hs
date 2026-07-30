module Ast.OnConflictSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.OnConflict
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  itSatisfiesIsAst @OnConflict
  itSatisfiesArbitrary @OnConflict
