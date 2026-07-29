module Ast.FconstSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.Fconst
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @Fconst
  itSatisfiesArbitrary @Fconst
