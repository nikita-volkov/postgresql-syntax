module Ast.JoinQualSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.JoinQual
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @JoinQual
  itSatisfiesArbitrary @JoinQual
