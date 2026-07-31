module PostgresqlSyntax.Ast.TrimList where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Algebra
import {-# SOURCE #-} PostgresqlSyntax.Ast.AExpr (AExpr)
import PostgresqlSyntax.Ast.ExprList
import qualified PostgresqlSyntax.Helpers.Gens as Gens
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
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
  toTextBuilder settings = \case
    ExprFromExprListTrimList a b -> toTextBuilder settings a <> " FROM " <> toTextBuilder settings b
    FromExprListTrimList a -> "FROM " <> toTextBuilder settings a
    ExprListTrimList a -> toTextBuilder settings a
  parser settings =
    asum
      [ ExprFromExprListTrimList <$> Parser.wrapToHead (parser settings) <*> (Parsers.space1 *> Parsers.keyword "from" *> Parsers.space1 *> Parser.endHead *> parser settings),
        FromExprListTrimList <$> (Parsers.keyword "from" *> Parsers.space1 *> Parser.endHead *> parser settings),
        ExprListTrimList <$> parser settings
      ]

instance Qc.Arbitrary TrimList where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.oneof
      [ ExprFromExprListTrimList <$> Gens.downscale Qc.arbitrary <*> Qc.arbitrary,
        FromExprListTrimList <$> Qc.arbitrary,
        ExprListTrimList <$> Qc.arbitrary
      ]
