module PostgresqlSyntax.Ast.SubstrListFromFor where

import qualified HeadedMegaparsec as Parser
import {-# SOURCE #-} PostgresqlSyntax.Ast.AExpr (AExpr)
import qualified PostgresqlSyntax.Helpers.Gens as Gens
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
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
  toTextBuilder settings = \case
    FromForSubstrListFromFor a b -> "FROM " <> toTextBuilder settings a <> " FOR " <> toTextBuilder settings b
    ForFromSubstrListFromFor a b -> "FOR " <> toTextBuilder settings a <> " FROM " <> toTextBuilder settings b
    FromSubstrListFromFor a -> "FROM " <> toTextBuilder settings a
    ForSubstrListFromFor a -> "FOR " <> toTextBuilder settings a
  parser settings =
    asum
      [ do
          a <- substrFrom
          asum
            [ do
                b <- Parsers.space1 *> substrFor
                return (FromForSubstrListFromFor a b),
              return (FromSubstrListFromFor a)
            ],
        do
          a <- substrFor
          asum
            [ do
                b <- Parsers.space1 *> substrFrom
                return (ForFromSubstrListFromFor a b),
              return (ForSubstrListFromFor a)
            ]
      ]
    where
      substrFrom = Parsers.keyword "from" *> Parsers.space1 *> Parser.endHead *> parser settings
      substrFor = Parsers.keyword "for" *> Parsers.space1 *> Parser.endHead *> parser settings

instance Qc.Arbitrary SubstrListFromFor where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.oneof
      [ FromForSubstrListFromFor <$> Gens.downscale Qc.arbitrary <*> Gens.downscale Qc.arbitrary,
        ForFromSubstrListFromFor <$> Gens.downscale Qc.arbitrary <*> Gens.downscale Qc.arbitrary,
        FromSubstrListFromFor <$> Gens.downscale Qc.arbitrary,
        ForSubstrListFromFor <$> Gens.downscale Qc.arbitrary
      ]
