module Ast.ConstTypenameSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.ConstTypename
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  itSatisfiesIsAst @ConstTypename
  itSatisfiesArbitrary @ConstTypename
