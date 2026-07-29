module Ast.CaseExprSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.CaseExpr
import Test.Hspec

spec :: Spec
spec = fullSpec @CaseExpr
