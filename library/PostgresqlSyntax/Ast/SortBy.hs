module PostgresqlSyntax.Ast.SortBy where

import HeadedMegaparsec
import PostgresqlSyntax.Ast.AscDesc
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Ast.NullsOrder
import PostgresqlSyntax.Ast.QualAllOp
import {-# SOURCE #-} PostgresqlSyntax.Ast.AExpr (AExpr, filteredParser)
import PostgresqlSyntax.Extras.HeadedMegaparsec hiding (run)
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, sortBy, try)
import Test.QuickCheck (scale)

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
  toTextBuilder = \case
    UsingSortBy a b c -> toTextBuilder a <> " USING " <> toTextBuilder b <> suffixMaybe toTextBuilder c
    AscDescSortBy a b c -> toTextBuilder a <> suffixMaybe toTextBuilder b <> suffixMaybe toTextBuilder c
  parser = do
    a <- filteredParser ["using", "asc", "desc", "nulls"]
    asum
      [ do
          space1
          keyword "using"
          space1
          endHead
          b <- parser
          c <- optional (space1 *> parser)
          return (UsingSortBy a b c),
        do
          b <- optional (space1 *> parser)
          c <- optional (space1 *> parser)
          return (AscDescSortBy a b c)
      ]

instance Arbitrary SortBy where
  arbitrary =
    oneof
      [ UsingSortBy <$> scale (`div` 2) arbitrary <*> arbitrary <*> arbitrary,
        AscDescSortBy <$> scale (`div` 2) arbitrary <*> arbitrary <*> arbitrary
      ]
