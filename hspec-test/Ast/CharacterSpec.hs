module Ast.CharacterSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.Character
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @Character
  itSatisfiesArbitrary @Character
