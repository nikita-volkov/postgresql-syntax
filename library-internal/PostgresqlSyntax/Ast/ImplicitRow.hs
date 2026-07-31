module PostgresqlSyntax.Ast.ImplicitRow where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Algebra
import {-# SOURCE #-} PostgresqlSyntax.Ast.AExpr (AExpr)
import PostgresqlSyntax.Ast.ExprList
import qualified PostgresqlSyntax.Extras.NonEmpty as NonEmpty
import qualified PostgresqlSyntax.Helpers.Gens as Gens
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
import PostgresqlSyntax.Prelude
import qualified Test.QuickCheck as Qc

-- |
-- ==== References
-- @
-- implicit_row:
--   | '(' expr_list ',' a_expr ')'
-- @
data ImplicitRow = ImplicitRow ExprList AExpr
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst ImplicitRow where
  toTextBuilder settings (ImplicitRow a b) = TextBuilders.renderInParens (toTextBuilder settings a <> ", " <> toTextBuilder settings b)

  -- Parses the shared @a_expr@ once and then decides, from what follows,
  -- whether it's the sole element of the leading 'ExprList' or the trailing
  -- @a_expr@ — see 'PostgresqlSyntax.Extras.NonEmpty.consAndUnsnoc'.
  parser settings = Parsers.inParens $ do
    a <- Parser.wrapToHead (parser settings)
    Parsers.commaSeparator
    b <- Parsers.sep1 Parsers.commaSeparator (parser settings)
    return $ case NonEmpty.consAndUnsnoc a b of
      (c, d) -> ImplicitRow (ExprList c) d

instance Qc.Arbitrary ImplicitRow where
  shrink = Qc.genericShrink
  arbitrary = ImplicitRow <$> arbitrary <*> Gens.downscale arbitrary
