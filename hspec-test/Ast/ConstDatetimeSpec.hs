module Ast.ConstDatetimeSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.ConstDatetime
import Test.Hspec

spec :: Spec
spec = fullSpec @ConstDatetime
