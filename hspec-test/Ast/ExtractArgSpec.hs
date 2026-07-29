module Ast.ExtractArgSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.ExtractArg
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @ExtractArg
  itSatisfiesArbitrary @ExtractArg
