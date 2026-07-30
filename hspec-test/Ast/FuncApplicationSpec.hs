module Ast.FuncApplicationSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.FuncApplication
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  itSatisfiesIsAst @FuncApplication
  itSatisfiesArbitrary @FuncApplication
