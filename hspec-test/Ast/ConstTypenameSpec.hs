module Ast.ConstTypenameSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.ConstTypename
import Test.Hspec

spec :: Spec
spec = fullSpec @ConstTypename
