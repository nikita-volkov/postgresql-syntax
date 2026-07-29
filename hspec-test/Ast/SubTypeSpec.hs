module Ast.SubTypeSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.SubType
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @SubType
  itSatisfiesArbitrary @SubType
