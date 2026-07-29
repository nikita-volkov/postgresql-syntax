module Ast.AscDescSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.AscDesc
import Test.Hspec

spec :: Spec
spec = fullSpec @AscDesc
