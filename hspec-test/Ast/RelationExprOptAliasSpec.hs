module Ast.RelationExprOptAliasSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.RelationExprOptAlias
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @RelationExprOptAlias
  itSatisfiesArbitrary @RelationExprOptAlias
