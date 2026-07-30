module Ast.SubTypeSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.SubType
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @SubType
  itSatisfiesArbitrary @SubType
