module Ast.CharacterSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.Character
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  itSatisfiesIsAst @Character
  itSatisfiesArbitrary @Character
