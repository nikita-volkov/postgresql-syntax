module Ast.AexprConstSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.AexprConst
import Test.Hspec

spec :: Spec
spec = fullSpec @AexprConst
