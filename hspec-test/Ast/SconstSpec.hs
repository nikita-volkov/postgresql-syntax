module Ast.SconstSpec (spec) where

import Helpers.Specs
import PostgresqlSyntax.Ast.Sconst
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  itSatisfiesIsAst @Sconst
  itSatisfiesArbitrary @Sconst
  describe "Parsers" $ do
    itParses @Sconst "'it''s good'"
    itParses @Sconst "$$it's good$$"
    itParses @Sconst "$x$it's good$x$"
