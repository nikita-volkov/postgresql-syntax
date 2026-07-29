module Ast.ArrayBoundsSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.ArrayBounds
import Test.Hspec

spec :: Spec
spec = fullSpec @ArrayBounds
