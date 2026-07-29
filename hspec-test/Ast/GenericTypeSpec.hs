module Ast.GenericTypeSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.GenericType
import Test.Hspec

spec :: Spec
spec = fullSpec @GenericType
