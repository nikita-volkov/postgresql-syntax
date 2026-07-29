module Ast.NameListSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.NameList
import Test.Hspec

spec :: Spec
spec = fullSpec @NameList
