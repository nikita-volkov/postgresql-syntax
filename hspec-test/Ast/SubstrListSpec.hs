module Ast.SubstrListSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.SubstrList
import Test.Hspec

spec :: Spec
spec = fullSpec @SubstrList
