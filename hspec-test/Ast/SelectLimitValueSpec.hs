module Ast.SelectLimitValueSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.SelectLimitValue
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @SelectLimitValue
  itSatisfiesArbitrary @SelectLimitValue
