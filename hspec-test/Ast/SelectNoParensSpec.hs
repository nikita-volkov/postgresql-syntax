module Ast.SelectNoParensSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.SelectNoParens
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @SelectNoParens
  itSatisfiesArbitrary @SelectNoParens
