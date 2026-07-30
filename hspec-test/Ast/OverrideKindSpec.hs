module Ast.OverrideKindSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.OverrideKind
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  itSatisfiesIsAst @OverrideKind
  itSatisfiesArbitrary @OverrideKind
