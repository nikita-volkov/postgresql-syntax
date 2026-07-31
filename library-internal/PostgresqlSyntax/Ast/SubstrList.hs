module PostgresqlSyntax.Ast.SubstrList where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Algebra
import {-# SOURCE #-} PostgresqlSyntax.Ast.AExpr (AExpr)
import PostgresqlSyntax.Ast.ExprList
import PostgresqlSyntax.Ast.SubstrListFromFor
import qualified PostgresqlSyntax.Helpers.Gens as Gens
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import PostgresqlSyntax.Prelude
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
  toTextBuilder settings = \case
    ExprSubstrList a b -> toTextBuilder settings a <> " " <> toTextBuilder settings b
    ExprListSubstrList a -> toTextBuilder settings a
  parser settings =
    asum
      [ ExprSubstrList <$> Parser.wrapToHead (parser settings) <*> (Parsers.space1 *> parser settings),
        ExprListSubstrList <$> parser settings
      ]

instance Qc.Arbitrary SubstrList where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.oneof
      [ ExprSubstrList <$> Gens.downscale Qc.arbitrary <*> Qc.arbitrary,
        ExprListSubstrList <$> Qc.arbitrary
      ]
