module Ast.SortClauseSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.SortClause
import Test.Hspec

spec :: Spec
spec = fullSpec @SortClause
