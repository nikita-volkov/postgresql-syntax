module PostgresqlSyntax.Ast.SubstrListFromFor where

import qualified HeadedMegaparsec as Parser
import {-# SOURCE #-} PostgresqlSyntax.Ast.AExpr (AExpr)
import PostgresqlSyntax.Ast.Internal
import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Parser
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude
import qualified Test.QuickCheck as Qc

-- |
-- ==== References
-- @
--   | a_expr substr_from substr_for
--   | a_expr substr_for substr_from
--   | a_expr substr_from
--   | a_expr substr_for
-- @
data SubstrListFromFor
  = FromForSubstrListFromFor AExpr AExpr
  | ForFromSubstrListFromFor AExpr AExpr
  | FromSubstrListFromFor AExpr
  | ForSubstrListFromFor AExpr
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst SubstrListFromFor where
  toTextBuilder = \case
    FromForSubstrListFromFor a b -> "FROM " <> toTextBuilder a <> " FOR " <> toTextBuilder b
    ForFromSubstrListFromFor a b -> "FOR " <> toTextBuilder a <> " FROM " <> toTextBuilder b
    FromSubstrListFromFor a -> "FROM " <> toTextBuilder a
    ForSubstrListFromFor a -> "FOR " <> toTextBuilder a
  parser =
    asum
      [ do
          a <- substrFrom
          asum
            [ do
                b <- Parser.space1 *> substrFor
                return (FromForSubstrListFromFor a b),
              return (FromSubstrListFromFor a)
            ],
        do
          a <- substrFor
          asum
            [ do
                b <- Parser.space1 *> substrFrom
                return (ForFromSubstrListFromFor a b),
              return (ForSubstrListFromFor a)
            ]
      ]
    where
      substrFrom = keyword "from" *> Parser.space1 *> Parser.endHead *> parser
      substrFor = keyword "for" *> Parser.space1 *> Parser.endHead *> parser

instance Qc.Arbitrary SubstrListFromFor where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.oneof
      [ FromForSubstrListFromFor <$> Qc.scale (`div` 2) Qc.arbitrary <*> Qc.scale (`div` 2) Qc.arbitrary,
        ForFromSubstrListFromFor <$> Qc.scale (`div` 2) Qc.arbitrary <*> Qc.scale (`div` 2) Qc.arbitrary,
        FromSubstrListFromFor <$> Qc.scale (`div` 2) Qc.arbitrary,
        ForSubstrListFromFor <$> Qc.scale (`div` 2) Qc.arbitrary
      ]
