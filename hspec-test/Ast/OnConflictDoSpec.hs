module Ast.OnConflictDoSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.OnConflictDo
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  itSatisfiesIsAst @OnConflictDo
  itSatisfiesArbitrary @OnConflictDo
