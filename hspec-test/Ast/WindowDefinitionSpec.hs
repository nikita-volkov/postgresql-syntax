module Ast.WindowDefinitionSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.WindowDefinition
import Test.Hspec

spec :: Spec
spec = fullSpec @WindowDefinition
