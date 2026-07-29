module Ast.QualAllOpSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.QualAllOp
import Test.Hspec

spec :: Spec
spec = fullSpec @QualAllOp
