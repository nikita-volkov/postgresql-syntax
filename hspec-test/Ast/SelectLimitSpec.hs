module Ast.SelectLimitSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.SelectLimit
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @SelectLimit
  itSatisfiesArbitrary @SelectLimit
