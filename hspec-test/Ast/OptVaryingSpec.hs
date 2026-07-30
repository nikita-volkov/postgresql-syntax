module Ast.OptVaryingSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.OptVarying
import Test.Hspec

spec :: Spec
spec = itSatisfiesArbitrary @OptVarying
