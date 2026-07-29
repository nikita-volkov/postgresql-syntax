module Ast.FuncApplicationSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.FuncApplication
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @FuncApplication
  itSatisfiesArbitrary @FuncApplication
