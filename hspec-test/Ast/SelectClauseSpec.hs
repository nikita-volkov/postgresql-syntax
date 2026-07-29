module Ast.SelectClauseSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.SelectClause
import Test.Hspec

spec :: Spec
spec = fullSpec @SelectClause
