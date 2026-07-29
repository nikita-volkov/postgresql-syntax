module Ast.OffsetClauseSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.OffsetClause
import Test.Hspec

spec :: Spec
spec = fullSpec @OffsetClause
