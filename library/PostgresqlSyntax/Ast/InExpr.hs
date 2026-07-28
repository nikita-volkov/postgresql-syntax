module PostgresqlSyntax.Ast.InExpr where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Ast.ExprList
import {-# SOURCE #-} PostgresqlSyntax.Ast.SelectWithParens (SelectWithParens)
import qualified PostgresqlSyntax.Helpers.Gens as Gens
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
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
    ExprListInExpr a -> TextBuilders.renderInParens (toTextBuilder a)
  parser =
    (ExprListInExpr <$> Parser.parse (Megaparsec.try (Parser.toParsec (Parsers.inParens parser))))
      <|> (SelectInExpr <$> Parser.wrapToHead parser)

instance Qc.Arbitrary InExpr where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.sized $ \n ->
      if n <= 1
        then ExprListInExpr <$> Qc.arbitrary
        else
          Qc.oneof
            [ SelectInExpr <$> Gens.downscale Qc.arbitrary,
              ExprListInExpr <$> Qc.arbitrary
            ]
