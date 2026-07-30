module Ast.WhereOrCurrentClauseSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.WhereOrCurrentClause
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @WhereOrCurrentClause
  itSatisfiesArbitrary @WhereOrCurrentClause
