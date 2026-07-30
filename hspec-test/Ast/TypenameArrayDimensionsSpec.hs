module Ast.TypenameArrayDimensionsSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.TypenameArrayDimensions
import Test.Hspec

spec :: Spec
spec = itSatisfiesArbitrary @TypenameArrayDimensions
