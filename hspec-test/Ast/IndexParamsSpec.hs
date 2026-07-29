module Ast.IndexParamsSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.IndexParams
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @IndexParams
  itSatisfiesArbitrary @IndexParams
