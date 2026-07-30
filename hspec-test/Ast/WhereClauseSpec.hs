module Ast.WhereClauseSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.WhereClause
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  itSatisfiesIsAst @WhereClause
  itSatisfiesArbitrary @WhereClause
