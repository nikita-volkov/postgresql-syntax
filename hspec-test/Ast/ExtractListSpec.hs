module Ast.ExtractListSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.ExtractList
import Test.Hspec

spec :: Spec
spec = fullSpec @ExtractList
