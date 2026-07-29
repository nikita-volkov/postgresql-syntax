module Ast.IndexElemDefSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.IndexElemDef
import Test.Hspec

spec :: Spec
spec = fullSpec @IndexElemDef
