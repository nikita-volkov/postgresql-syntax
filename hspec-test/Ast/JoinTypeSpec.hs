module Ast.JoinTypeSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.JoinType
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  itSatisfiesIsAst @JoinType
  itSatisfiesArbitrary @JoinType
