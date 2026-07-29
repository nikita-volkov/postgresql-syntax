module Ast.SubTypeSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.SubType
import Test.Hspec

spec :: Spec
spec = fullSpec @SubType
