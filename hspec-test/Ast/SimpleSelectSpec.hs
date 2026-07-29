module Ast.SimpleSelectSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.SimpleSelect
import Test.Hspec

spec :: Spec
spec = fullSpec @SimpleSelect
