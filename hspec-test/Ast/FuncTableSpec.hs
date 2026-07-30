module Ast.FuncTableSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.FuncTable
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  itSatisfiesIsAst @FuncTable
  itSatisfiesArbitrary @FuncTable
