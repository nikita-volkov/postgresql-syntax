module Ast.AttrsSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.Attrs
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @Attrs
  itSatisfiesArbitrary @Attrs
