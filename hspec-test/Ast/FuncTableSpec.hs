module Ast.FuncTableSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.FuncTable
import Test.Hspec

spec :: Spec
spec = fullSpec @FuncTable
