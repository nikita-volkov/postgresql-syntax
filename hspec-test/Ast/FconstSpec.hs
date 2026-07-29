module Ast.FconstSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.Fconst
import Test.Hspec

spec :: Spec
spec = fullSpec @Fconst
