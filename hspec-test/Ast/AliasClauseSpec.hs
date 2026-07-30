module Ast.AliasClauseSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.AliasClause
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  itSatisfiesIsAst @AliasClause
  itSatisfiesArbitrary @AliasClause
