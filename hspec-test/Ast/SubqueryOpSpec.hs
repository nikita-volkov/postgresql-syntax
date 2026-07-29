module Ast.SubqueryOpSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.SubqueryOp
import Test.Hspec

spec :: Spec
spec = fullSpec @SubqueryOp
