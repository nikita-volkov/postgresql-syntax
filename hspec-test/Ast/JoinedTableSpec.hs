module Ast.JoinedTableSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.JoinedTable
import Test.Hspec

spec :: Spec
spec = fullSpec @JoinedTable
