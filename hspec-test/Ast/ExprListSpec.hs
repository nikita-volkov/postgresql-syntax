module Ast.ExprListSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.ExprList
import Test.Hspec

spec :: Spec
spec = fullSpec @ExprList
