module Ast.IndexParamsSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.IndexParams
import Test.Hspec

spec :: Spec
spec = fullSpec @IndexParams
