module Ast.SubTypeSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.SubType
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  itSatisfiesIsAst @SubType
  itSatisfiesArbitrary @SubType
