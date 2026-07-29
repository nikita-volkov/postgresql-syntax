module Ast.PositionListSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.PositionList
import Test.Hspec

spec :: Spec
spec = fullSpec @PositionList
