module Ast.IntervalSecondSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.IntervalSecond
import Test.Hspec

spec :: Spec
spec = fullSpec @IntervalSecond
