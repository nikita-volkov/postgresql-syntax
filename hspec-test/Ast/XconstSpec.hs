module Ast.XconstSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.Xconst
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  itSatisfiesIsAst @Xconst
  itSatisfiesArbitrary @Xconst
