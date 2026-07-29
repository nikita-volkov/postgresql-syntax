module Ast.GenericTypeSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.GenericType
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @GenericType
  itSatisfiesArbitrary @GenericType
