module Ast.FuncApplicationSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.FuncApplication
import Test.Hspec

spec :: Spec
spec = fullSpec @FuncApplication
