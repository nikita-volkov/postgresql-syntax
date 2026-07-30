module Ast.ExtractArgSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.ExtractArg
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  itSatisfiesIsAst @ExtractArg
  itSatisfiesArbitrary @ExtractArg
