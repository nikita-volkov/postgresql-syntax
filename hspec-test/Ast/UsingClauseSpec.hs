module Ast.UsingClauseSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.UsingClause
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  itSatisfiesIsAst @UsingClause
  itSatisfiesArbitrary @UsingClause
