module Ast.SelectBinOpSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.SelectBinOp
import Test.Hspec

spec :: Spec
spec = fullSpec @SelectBinOp
