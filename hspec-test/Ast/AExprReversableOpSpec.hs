module Ast.AExprReversableOpSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.AExprReversableOp
import Test.Hspec

spec :: Spec
spec = fullSpec @AExprReversableOp
