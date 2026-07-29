module Ast.FuncConstArgsSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.FuncConstArgs
import Test.Hspec

spec :: Spec
spec = fullSpec @FuncConstArgs
