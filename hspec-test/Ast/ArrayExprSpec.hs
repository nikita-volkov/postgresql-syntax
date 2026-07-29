module Ast.ArrayExprSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.ArrayExpr
import Test.Hspec

spec :: Spec
spec = fullSpec @ArrayExpr
