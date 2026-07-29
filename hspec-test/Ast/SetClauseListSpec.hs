module Ast.SetClauseListSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.SetClauseList
import Test.Hspec

spec :: Spec
spec = fullSpec @SetClauseList
