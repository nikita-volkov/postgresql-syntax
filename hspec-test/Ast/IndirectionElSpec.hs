module Ast.IndirectionElSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.IndirectionEl
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @IndirectionEl
  itSatisfiesArbitrary @IndirectionEl
