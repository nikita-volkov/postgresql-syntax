module PostgresqlSyntax.Ast.OverClause where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Ast.Ident
import PostgresqlSyntax.Ast.WindowSpecification
import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Parser
import PostgresqlSyntax.Ast.Internal
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
  toTextBuilder = \case
    WindowOverClause a -> "OVER " <> toTextBuilder a
    ColIdOverClause a -> "OVER " <> toTextBuilder a
  parser = do
    keyword "over"
    Parser.space1
    Parser.endHead
    asum
      [ WindowOverClause <$> parser,
        ColIdOverClause <$> colId
      ]

instance Qc.Arbitrary OverClause where
  arbitrary =
    Qc.oneof
      [ WindowOverClause <$> Qc.scale (`div` 2) Qc.arbitrary,
        ColIdOverClause <$> Qc.arbitrary
      ]
