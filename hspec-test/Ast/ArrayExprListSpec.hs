module Ast.ArrayExprListSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.ArrayExprList
import Test.Hspec

spec :: Spec
spec = fullSpec @ArrayExprList
