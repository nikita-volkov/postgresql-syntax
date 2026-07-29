module Ast.TrimListSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.TrimList
import Test.Hspec

spec :: Spec
spec = fullSpec @TrimList
