module Ast.TypenameArrayDimensionsSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.TypenameArrayDimensions
import Test.Hspec

spec :: Spec
spec = onlyArbitrarySpec @TypenameArrayDimensions
