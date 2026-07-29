module Ast.InExprSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.InExpr
import Test.Hspec

spec :: Spec
spec = fullSpec @InExpr
