module PostgresqlSyntax.Ast.ImplicitRow where

import qualified HeadedMegaparsec as Parser
import {-# SOURCE #-} PostgresqlSyntax.Ast.AExpr (AExpr)
import PostgresqlSyntax.Ast.ExprList
import qualified PostgresqlSyntax.Extras.NonEmpty as NonEmpty
import qualified PostgresqlSyntax.Extras.QuickCheck as Qc
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
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
  toTextBuilder (ImplicitRow a b) = TextBuilders.renderInParens (toTextBuilder a <> ", " <> toTextBuilder b)

  -- Parses the shared @a_expr@ once and then decides, from what follows,
  -- whether it's the sole element of the leading 'ExprList' or the trailing
  -- @a_expr@ — see 'PostgresqlSyntax.Extras.NonEmpty.consAndUnsnoc'.
  parser = Parsers.inParens $ do
    a <- Parser.wrapToHead parser
    Parsers.commaSeparator
    b <- Parsers.sep1 Parsers.commaSeparator parser
    return $ case NonEmpty.consAndUnsnoc a b of
      (c, d) -> ImplicitRow (ExprList c) d

instance Qc.Arbitrary ImplicitRow where
  shrink = Qc.genericShrink
  arbitrary = ImplicitRow <$> arbitrary <*> Qc.downscale arbitrary
