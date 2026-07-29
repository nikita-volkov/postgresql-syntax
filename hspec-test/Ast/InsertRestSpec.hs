module Ast.InsertRestSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.InsertRest
import Test.Hspec

spec :: Spec
spec = fullSpec @InsertRest
