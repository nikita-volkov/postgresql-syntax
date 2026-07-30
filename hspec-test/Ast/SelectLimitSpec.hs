module Ast.SelectLimitSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.SelectLimit
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  itSatisfiesIsAst @SelectLimit
  itSatisfiesArbitrary @SelectLimit
