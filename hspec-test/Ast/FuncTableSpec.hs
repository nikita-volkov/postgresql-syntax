module Ast.FuncTableSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.FuncTable
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @FuncTable
  itSatisfiesArbitrary @FuncTable
