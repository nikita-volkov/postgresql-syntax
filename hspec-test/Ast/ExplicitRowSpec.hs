module Ast.ExplicitRowSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.ExplicitRow
import Test.Hspec

spec :: Spec
spec = fullSpec @ExplicitRow
