module Ast.WindowSpecificationSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.WindowSpecification
import Test.Hspec

spec :: Spec
spec = fullSpec @WindowSpecification
