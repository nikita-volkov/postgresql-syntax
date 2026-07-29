module Ast.ConstCharacterSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.ConstCharacter
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @ConstCharacter
  itSatisfiesArbitrary @ConstCharacter
