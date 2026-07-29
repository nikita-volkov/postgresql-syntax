module Ast.SconstSpec (spec) where

import Helpers
import PostgresqlSyntax.Ast.Sconst
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  fullSpec @Sconst
  describe "Parsers" $ do
    it "sconst"
      $ forM_
        [ "'it''s good'",
          "$$it's good$$",
          "$x$it's good$x$"
        ]
        (parsesTo @Sconst)
