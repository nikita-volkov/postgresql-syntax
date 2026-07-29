module Ast.AttrsSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.Attrs
import Test.Hspec

spec :: Spec
spec = fullSpec @Attrs
