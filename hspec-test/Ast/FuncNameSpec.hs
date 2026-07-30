module Ast.FuncNameSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.FuncName
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  itSatisfiesIsAst @FuncName
  itSatisfiesArbitrary @FuncName
