module Ast.QualifiedNameSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.QualifiedName
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @QualifiedName
  itSatisfiesArbitrary @QualifiedName
