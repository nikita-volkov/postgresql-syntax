module Ast.QualOpSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.QualOp
import Test.Hspec

spec :: Spec
spec = fullSpec @QualOp
