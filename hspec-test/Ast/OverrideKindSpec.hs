module Ast.OverrideKindSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.OverrideKind
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @OverrideKind
  itSatisfiesArbitrary @OverrideKind
