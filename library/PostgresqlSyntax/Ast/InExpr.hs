module PostgresqlSyntax.Ast.InExpr where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Ast.ExprList
import PostgresqlSyntax.Ast.Internal
import {-# SOURCE #-} PostgresqlSyntax.Ast.SelectWithParens (SelectWithParens)
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import qualified Test.QuickCheck as Qc
import qualified Text.Megaparsec as Megaparsec

-- |
-- ==== References
-- @
-- in_expr:
--   | select_with_parens
--   | '(' expr_list ')'
-- @
data InExpr
  = SelectInExpr SelectWithParens
  | ExprListInExpr ExprList
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst InExpr where
  toTextBuilder = \case
    SelectInExpr a -> toTextBuilder a
    ExprListInExpr a -> renderInParens (toTextBuilder a)
  parser =
    (ExprListInExpr <$> Parser.parse (Megaparsec.try (Parser.toParsec (inParens parser))))
      <|> (SelectInExpr <$> Parser.wrapToHead parser)

instance Qc.Arbitrary InExpr where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.sized $ \n ->
      if n <= 1
        then ExprListInExpr <$> Qc.arbitrary
        else
          Qc.oneof
            [ SelectInExpr <$> Qc.scale (`div` 2) Qc.arbitrary,
              ExprListInExpr <$> Qc.scale (`div` 2) Qc.arbitrary
            ]
