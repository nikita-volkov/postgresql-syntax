module Ast.WindowExclusionClauseSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.WindowExclusionClause
import Test.Hspec

spec :: Spec
spec = fullSpec @WindowExclusionClause
