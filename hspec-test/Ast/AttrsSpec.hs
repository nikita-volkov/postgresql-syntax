module Ast.AttrsSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.Attrs
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  itSatisfiesIsAst @Attrs
  itSatisfiesArbitrary @Attrs
