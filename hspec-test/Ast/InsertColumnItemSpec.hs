module Ast.InsertColumnItemSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.InsertColumnItem
import Test.Hspec

spec :: Spec
spec = fullSpec @InsertColumnItem
