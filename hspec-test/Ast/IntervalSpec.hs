module Ast.IntervalSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.Interval
import Test.Hspec

spec :: Spec
spec = fullSpec @Interval
