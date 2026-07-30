module Ast.BconstSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.Bconst
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  itSatisfiesIsAst @Bconst
  itSatisfiesArbitrary @Bconst
