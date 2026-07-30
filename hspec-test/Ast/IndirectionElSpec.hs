module Ast.IndirectionElSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.IndirectionEl
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  itSatisfiesIsAst @IndirectionEl
  itSatisfiesArbitrary @IndirectionEl
