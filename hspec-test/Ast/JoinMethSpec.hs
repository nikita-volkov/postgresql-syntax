module Ast.JoinMethSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.JoinMeth
import Test.Hspec

spec :: Spec
spec = fullSpec @JoinMeth
