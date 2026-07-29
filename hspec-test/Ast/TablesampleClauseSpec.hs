module Ast.TablesampleClauseSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.TablesampleClause
import Test.Hspec

spec :: Spec
spec = fullSpec @TablesampleClause
