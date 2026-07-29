module Ast.SelectLimitValueSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.SelectLimitValue
import Test.Hspec

spec :: Spec
spec = fullSpec @SelectLimitValue
