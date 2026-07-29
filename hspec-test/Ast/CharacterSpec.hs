module Ast.CharacterSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.Character
import Test.Hspec

spec :: Spec
spec = fullSpec @Character
