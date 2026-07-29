module Ast.ExtractArgSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.ExtractArg
import Test.Hspec

spec :: Spec
spec = fullSpec @ExtractArg
