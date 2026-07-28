module PostgresqlSyntax.Ast.TrimList where

import qualified HeadedMegaparsec as Parser
import {-# SOURCE #-} PostgresqlSyntax.Ast.AExpr (AExpr)
import PostgresqlSyntax.Ast.ExprList
import qualified PostgresqlSyntax.Extras.QuickCheck as Qc
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude
import qualified Test.QuickCheck as Qc

-- |
-- ==== References
-- @
-- trim_list:
--   | a_expr FROM expr_list
--   | FROM expr_list
--   | expr_list
-- @
data TrimList
  = ExprFromExprListTrimList AExpr ExprList
  | FromExprListTrimList ExprList
  | ExprListTrimList ExprList
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst TrimList where
  toTextBuilder = \case
    ExprFromExprListTrimList a b -> toTextBuilder a <> " FROM " <> toTextBuilder b
    FromExprListTrimList a -> "FROM " <> toTextBuilder a
    ExprListTrimList a -> toTextBuilder a
  parser =
    asum
      [ ExprFromExprListTrimList <$> Parser.wrapToHead parser <*> (Parsers.space1 *> Parsers.keyword "from" *> Parsers.space1 *> Parser.endHead *> parser),
        FromExprListTrimList <$> (Parsers.keyword "from" *> Parsers.space1 *> Parser.endHead *> parser),
        ExprListTrimList <$> parser
      ]

instance Qc.Arbitrary TrimList where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.oneof
      [ ExprFromExprListTrimList <$> Qc.downscale Qc.arbitrary <*> Qc.arbitrary,
        FromExprListTrimList <$> Qc.arbitrary,
        ExprListTrimList <$> Qc.arbitrary
      ]
