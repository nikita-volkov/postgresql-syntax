module PostgresqlSyntax.Ast.OverClause where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Ast.Ident
import PostgresqlSyntax.Ast.WindowSpecification
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude
import qualified Test.QuickCheck as Qc

-- |
-- ==== References
-- @
-- over_clause:
--   | OVER window_specification
--   | OVER ColId
--   | EMPTY
-- @
data OverClause
  = WindowOverClause WindowSpecification
  | ColIdOverClause Ident
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst OverClause where
  toTextBuilder settings = \case
    WindowOverClause a -> "OVER " <> toTextBuilder settings a
    ColIdOverClause a -> "OVER " <> toTextBuilder settings a
  parser settings = do
    Parsers.keyword "over"
    Parsers.space1
    Parser.endHead
    asum
      [ WindowOverClause <$> parser settings,
        ColIdOverClause <$> colId settings
      ]

instance Qc.Arbitrary OverClause where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.oneof
      [ WindowOverClause <$> Qc.arbitrary,
        ColIdOverClause <$> Qc.arbitrary
      ]
