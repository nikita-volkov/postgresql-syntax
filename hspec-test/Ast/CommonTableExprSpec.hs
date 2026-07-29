module Ast.CommonTableExprSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.CommonTableExpr
import Test.Hspec

spec :: Spec
spec = fullSpec @CommonTableExpr
