module Ast.ConstDatetimeSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.ConstDatetime
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @ConstDatetime
  itSatisfiesArbitrary @ConstDatetime
