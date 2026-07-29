module Ast.ForLockingClauseSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.ForLockingClause
import Test.Hspec

spec :: Spec
spec = fullSpec @ForLockingClause
