module Ast.IconstSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.Iconst
import Test.Hspec

spec :: Spec
spec = fullSpec @Iconst
