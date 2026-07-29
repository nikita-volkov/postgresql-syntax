module Ast.CExprSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.CExpr
import Test.Hspec

spec :: Spec
spec = fullSpec @CExpr
