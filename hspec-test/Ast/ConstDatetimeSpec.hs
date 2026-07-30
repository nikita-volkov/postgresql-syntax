module Ast.ConstDatetimeSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.ConstDatetime
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  itSatisfiesIsAst @ConstDatetime
  itSatisfiesArbitrary @ConstDatetime
