module Ast.ConstCharacterSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.ConstCharacter
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  itSatisfiesIsAst @ConstCharacter
  itSatisfiesArbitrary @ConstCharacter
