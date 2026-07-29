module Ast.RowSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.Row
import Test.Hspec

spec :: Spec
spec = fullSpec @Row
