module Ast.BExprIsOpSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.BExprIsOp
import Test.Hspec

spec :: Spec
spec = fullSpec @BExprIsOp
