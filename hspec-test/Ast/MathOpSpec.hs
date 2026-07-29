module Ast.MathOpSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.MathOp
import Test.Hspec

spec :: Spec
spec = fullSpec @MathOp
