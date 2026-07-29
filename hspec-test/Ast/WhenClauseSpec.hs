module Ast.WhenClauseSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.WhenClause
import Test.Hspec

spec :: Spec
spec = fullSpec @WhenClause
