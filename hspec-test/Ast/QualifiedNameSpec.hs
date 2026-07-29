module Ast.QualifiedNameSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.QualifiedName
import Test.Hspec

spec :: Spec
spec = fullSpec @QualifiedName
