module Ast.TypeListSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.TypeList
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  itSatisfiesIsAst @TypeList
  itSatisfiesArbitrary @TypeList
