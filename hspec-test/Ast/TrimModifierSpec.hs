module Ast.TrimModifierSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.TrimModifier
import Test.Hspec

spec :: Spec
spec = fullSpec @TrimModifier
