module Ast.QualifiedNameSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.QualifiedName
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  itSatisfiesIsAst @QualifiedName
  itSatisfiesArbitrary @QualifiedName
