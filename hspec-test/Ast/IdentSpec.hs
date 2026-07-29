module Ast.IdentSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.Ident
import Test.Hspec

spec :: Spec
spec = fullSpec @Ident
