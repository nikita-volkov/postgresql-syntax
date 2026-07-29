module Ast.InsertColumnListSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.InsertColumnList
import Test.Hspec

spec :: Spec
spec = fullSpec @InsertColumnList
