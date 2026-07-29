module Ast.FuncNameSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.FuncName
import Test.Hspec

spec :: Spec
spec = fullSpec @FuncName
