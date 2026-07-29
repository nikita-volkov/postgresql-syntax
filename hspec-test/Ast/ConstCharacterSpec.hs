module Ast.ConstCharacterSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.ConstCharacter
import Test.Hspec

spec :: Spec
spec = fullSpec @ConstCharacter
