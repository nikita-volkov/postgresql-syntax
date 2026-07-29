module Ast.XconstSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.Xconst
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @Xconst
  itSatisfiesArbitrary @Xconst
