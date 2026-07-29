module Ast.OptVaryingSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.OptVarying
import Test.Hspec

spec :: Spec
spec = onlyArbitrarySpec @OptVarying
