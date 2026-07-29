module Ast.SimpleTypenameSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.SimpleTypename
import Test.Hspec

spec :: Spec
spec = fullSpec @SimpleTypename
