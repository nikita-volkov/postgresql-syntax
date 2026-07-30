module Ast.GenericTypeSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.GenericType
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  itSatisfiesIsAst @GenericType
  itSatisfiesArbitrary @GenericType
