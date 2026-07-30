module Ast.FuncConstArgsSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.FuncConstArgs
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @FuncConstArgs
  itSatisfiesArbitrary @FuncConstArgs
