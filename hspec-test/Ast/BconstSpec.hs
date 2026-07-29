module Ast.BconstSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.Bconst
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @Bconst
  itSatisfiesArbitrary @Bconst
