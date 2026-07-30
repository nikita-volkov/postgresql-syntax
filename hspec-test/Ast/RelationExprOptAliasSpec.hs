module Ast.RelationExprOptAliasSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.RelationExprOptAlias
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  itSatisfiesIsAst @RelationExprOptAlias
  itSatisfiesArbitrary @RelationExprOptAlias
