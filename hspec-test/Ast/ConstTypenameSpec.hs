module Ast.ConstTypenameSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.ConstTypename
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @ConstTypename
  itSatisfiesArbitrary @ConstTypename
