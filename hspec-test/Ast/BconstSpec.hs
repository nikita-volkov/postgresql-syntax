module Ast.BconstSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.Bconst
import Test.Hspec

spec :: Spec
spec = fullSpec @Bconst
