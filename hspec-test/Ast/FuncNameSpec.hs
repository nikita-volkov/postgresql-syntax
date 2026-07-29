module Ast.FuncNameSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.FuncName
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @FuncName
  itSatisfiesArbitrary @FuncName
