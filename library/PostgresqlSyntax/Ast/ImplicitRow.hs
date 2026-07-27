module PostgresqlSyntax.Ast.ImplicitRow where

import HeadedMegaparsec
import PostgresqlSyntax.Ast.ExprList
import PostgresqlSyntax.Ast.Internal
import qualified PostgresqlSyntax.Extras.NonEmpty as NonEmpty
import {-# SOURCE #-} PostgresqlSyntax.Ast.AExpr (AExpr)
import PostgresqlSyntax.Extras.HeadedMegaparsec hiding (run)
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import Test.QuickCheck (scale)

-- |
-- ==== References
-- @
-- implicit_row:
--   | '(' expr_list ',' a_expr ')'
-- @
data ImplicitRow = ImplicitRow ExprList AExpr
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst ImplicitRow where
  toTextBuilder (ImplicitRow a b) = renderInParens (toTextBuilder a <> ", " <> toTextBuilder b)
  -- |
  -- Parses the shared @a_expr@ once and then decides, from what follows,
  -- whether it's the sole element of the leading 'ExprList' or the trailing
  -- @a_expr@ — see 'PostgresqlSyntax.Extras.NonEmpty.consAndUnsnoc'.
  parser = inParens $ do
    a <- wrapToHead parser
    commaSeparator
    b <- sep1 commaSeparator parser
    return $ case NonEmpty.consAndUnsnoc a b of
      (c, d) -> ImplicitRow (ExprList c) d

instance Arbitrary ImplicitRow where
  arbitrary = ImplicitRow <$> scale (`div` 2) arbitrary <*> scale (`div` 2) arbitrary
