module Ast.AnyOperatorSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.AnyOperator
import Test.Hspec

spec :: Spec
spec = fullSpec @AnyOperator
