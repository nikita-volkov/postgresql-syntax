module Ast.IndirectionSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.Indirection
import Test.Hspec

spec :: Spec
spec = fullSpec @Indirection
