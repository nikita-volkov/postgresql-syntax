module Ast.AllOpSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.AllOp
import Test.Hspec

spec :: Spec
spec = fullSpec @AllOp
