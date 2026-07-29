module Ast.SymbolicExprBinOpSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.SymbolicExprBinOp
import Test.Hspec

spec :: Spec
spec = fullSpec @SymbolicExprBinOp
