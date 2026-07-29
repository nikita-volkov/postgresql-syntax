module Ast.OptOrdinalitySpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.OptOrdinality
import Test.Hspec

spec :: Spec
spec = fullSpec @OptOrdinality
