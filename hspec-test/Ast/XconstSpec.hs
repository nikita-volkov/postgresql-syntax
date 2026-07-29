module Ast.XconstSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.Xconst
import Test.Hspec

spec :: Spec
spec = fullSpec @Xconst
