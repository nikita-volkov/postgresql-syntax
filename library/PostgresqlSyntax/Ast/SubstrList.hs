module PostgresqlSyntax.Ast.SubstrList where

import qualified HeadedMegaparsec as Parser
import {-# SOURCE #-} PostgresqlSyntax.Ast.AExpr (AExpr)
import PostgresqlSyntax.Ast.ExprList
import PostgresqlSyntax.Ast.SubstrListFromFor
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import qualified Test.QuickCheck as Qc

-- |
-- ==== References
-- @
-- substr_list:
--   | a_expr substr_from substr_for
--   | a_expr substr_for substr_from
--   | a_expr substr_from
--   | a_expr substr_for
--   | expr_list
--   | EMPTY
-- @
data SubstrList
  = ExprSubstrList AExpr SubstrListFromFor
  | ExprListSubstrList ExprList
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst SubstrList where
  toTextBuilder = \case
    ExprSubstrList a b -> toTextBuilder a <> " " <> toTextBuilder b
    ExprListSubstrList a -> toTextBuilder a
  parser =
    asum
      [ ExprSubstrList <$> Parser.wrapToHead parser <*> (Parsers.space1 *> parser),
        ExprListSubstrList <$> parser
      ]

instance Qc.Arbitrary SubstrList where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.oneof
      [ ExprSubstrList <$> Qc.scale (`div` 2) Qc.arbitrary <*> Qc.scale (`div` 2) Qc.arbitrary,
        ExprListSubstrList <$> Qc.scale (`div` 2) Qc.arbitrary
      ]
