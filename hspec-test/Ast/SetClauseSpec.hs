module Ast.SetClauseSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.SetClause
import Test.Hspec

spec :: Spec
spec = fullSpec @SetClause
