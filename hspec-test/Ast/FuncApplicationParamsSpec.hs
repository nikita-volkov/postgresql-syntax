module Ast.FuncApplicationParamsSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.FuncApplicationParams
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  itSatisfiesIsAst @FuncApplicationParams
  itSatisfiesArbitrary @FuncApplicationParams
