module PostgresqlSyntax.Ast.OverClause where

import HeadedMegaparsec
import PostgresqlSyntax.Ast.Ident
import PostgresqlSyntax.Ast.WindowSpecification
import PostgresqlSyntax.Extras.HeadedMegaparsec hiding (run)
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude
import Test.QuickCheck (scale)

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
    space1
    endHead
    asum
      [ WindowOverClause <$> parser,
        ColIdOverClause <$> colId
      ]

instance Arbitrary OverClause where
  arbitrary =
    oneof
      [ WindowOverClause <$> scale (`div` 2) arbitrary,
        ColIdOverClause <$> arbitrary
      ]
