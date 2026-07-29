module Ast.WhenClauseListSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.WhenClauseList
import Test.Hspec

spec :: Spec
spec = fullSpec @WhenClauseList
