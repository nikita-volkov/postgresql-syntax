module Ast.SelectLimitSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.SelectLimit
import Test.Hspec

spec :: Spec
spec = fullSpec @SelectLimit
