module Ast.FuncApplicationParamsSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.FuncApplicationParams
import Test.Hspec

spec :: Spec
spec = fullSpec @FuncApplicationParams
