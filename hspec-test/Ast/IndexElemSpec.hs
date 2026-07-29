module Ast.IndexElemSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.IndexElem
import Test.Hspec

spec :: Spec
spec = fullSpec @IndexElem
