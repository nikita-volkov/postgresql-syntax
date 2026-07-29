module Ast.WithClauseSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.WithClause
import Test.Hspec

spec :: Spec
spec = fullSpec @WithClause
