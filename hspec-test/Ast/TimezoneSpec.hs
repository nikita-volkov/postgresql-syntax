module Ast.TimezoneSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.Timezone
import Test.Hspec

spec :: Spec
spec = fullSpec @Timezone
