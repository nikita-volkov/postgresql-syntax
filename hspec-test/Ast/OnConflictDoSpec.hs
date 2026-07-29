module Ast.OnConflictDoSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.OnConflictDo
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @OnConflictDo
  itSatisfiesArbitrary @OnConflictDo
