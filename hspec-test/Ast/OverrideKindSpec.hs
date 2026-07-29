module Ast.OverrideKindSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.OverrideKind
import Test.Hspec

spec :: Spec
spec = fullSpec @OverrideKind
