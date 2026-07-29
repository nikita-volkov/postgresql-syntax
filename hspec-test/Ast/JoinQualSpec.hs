module Ast.JoinQualSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.JoinQual
import Test.Hspec

spec :: Spec
spec = fullSpec @JoinQual
