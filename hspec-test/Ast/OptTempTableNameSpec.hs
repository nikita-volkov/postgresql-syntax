module Ast.OptTempTableNameSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.OptTempTableName
import Test.Hspec

spec :: Spec
spec = fullSpec @OptTempTableName
