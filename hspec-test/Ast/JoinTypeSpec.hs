module Ast.JoinTypeSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.JoinType
import Test.Hspec

spec :: Spec
spec = fullSpec @JoinType
