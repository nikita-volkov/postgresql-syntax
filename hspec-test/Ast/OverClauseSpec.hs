module Ast.OverClauseSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.OverClause
import Test.Hspec

spec :: Spec
spec = fullSpec @OverClause
