module Ast.SelectNoParensSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.SelectNoParens
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  itSatisfiesIsAst @SelectNoParens
  itSatisfiesArbitrary @SelectNoParens
