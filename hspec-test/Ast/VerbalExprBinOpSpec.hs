module Ast.VerbalExprBinOpSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.VerbalExprBinOp
import Test.Hspec

spec :: Spec
spec = fullSpec @VerbalExprBinOp
