module Ast.JoinQualSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.JoinQual
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @JoinQual
  itSatisfiesArbitrary @JoinQual
