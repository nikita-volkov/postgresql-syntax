module Ast.SelectNoParensSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.SelectNoParens
import Test.Hspec

spec :: Spec
spec = fullSpec @SelectNoParens
