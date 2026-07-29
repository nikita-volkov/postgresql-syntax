module Ast.AnyNameSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.AnyName
import Test.Hspec

spec :: Spec
spec = fullSpec @AnyName
