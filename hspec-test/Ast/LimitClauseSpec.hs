module Ast.LimitClauseSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.LimitClause
import Test.Hspec

spec :: Spec
spec = fullSpec @LimitClause
