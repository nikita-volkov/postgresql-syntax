module Ast.ConfExprSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.ConfExpr
import Test.Hspec

spec :: Spec
spec = fullSpec @ConfExpr
