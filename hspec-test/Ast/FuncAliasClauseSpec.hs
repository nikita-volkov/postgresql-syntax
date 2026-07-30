module Ast.FuncAliasClauseSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.FuncAliasClause
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  itSatisfiesIsAst @FuncAliasClause
  itSatisfiesArbitrary @FuncAliasClause
