module PostgresqlSyntax.Ast.SortBy where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Algebra
import {-# SOURCE #-} PostgresqlSyntax.Ast.AExpr (AExpr)
import PostgresqlSyntax.Ast.AscDesc
import PostgresqlSyntax.Ast.NullsOrder
import PostgresqlSyntax.Ast.QualAllOp
import qualified PostgresqlSyntax.Helpers.Gens as Gens
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
import PostgresqlSyntax.Prelude hiding (filter, many, some, sortBy, try)
import qualified Test.QuickCheck as Qc

-- |
-- ==== References
-- @
-- sortby:
--   | a_expr USING qual_all_Op opt_nulls_order
--   | a_expr opt_asc_desc opt_nulls_order
-- @
data SortBy
  = UsingSortBy AExpr QualAllOp (Maybe NullsOrder)
  | AscDescSortBy AExpr (Maybe AscDesc) (Maybe NullsOrder)
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst SortBy where
  toTextBuilder settings = \case
    UsingSortBy a b c -> toTextBuilder settings a <> " USING " <> toTextBuilder settings b <> TextBuilders.suffixMaybe (toTextBuilder settings) c
    AscDescSortBy a b c -> toTextBuilder settings a <> TextBuilders.suffixMaybe (toTextBuilder settings) b <> TextBuilders.suffixMaybe (toTextBuilder settings) c
  parser settings = do
    a <- parser settings
    asum
      [ do
          Parsers.space1
          Parsers.keyword "using"
          Parsers.space1
          Parser.endHead
          b <- parser settings
          c <- optional (Parsers.space1 *> parser settings)
          return (UsingSortBy a b c),
        do
          b <- optional (Parsers.space1 *> parser settings)
          c <- optional (Parsers.space1 *> parser settings)
          return (AscDescSortBy a b c)
      ]

instance Qc.Arbitrary SortBy where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.oneof
      [ UsingSortBy <$> Gens.downscale Qc.arbitrary <*> Qc.arbitrary <*> Qc.arbitrary,
        AscDescSortBy <$> Gens.downscale Qc.arbitrary <*> Qc.arbitrary <*> Qc.arbitrary
      ]
