module Ast.TypeListSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.TypeList
import Test.Hspec

spec :: Spec
spec = fullSpec @TypeList
