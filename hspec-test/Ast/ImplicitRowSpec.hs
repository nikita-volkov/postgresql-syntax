module Ast.ImplicitRowSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.ImplicitRow
import Test.Hspec

spec :: Spec
spec = fullSpec @ImplicitRow
