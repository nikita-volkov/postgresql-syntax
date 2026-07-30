module Ast.SimpleTypenameSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.SimpleTypename
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  itSatisfiesIsAst @SimpleTypename
  itSatisfiesArbitrary @SimpleTypename
