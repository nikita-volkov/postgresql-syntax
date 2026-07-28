module PostgresqlSyntax.Ast.SortBy where

import qualified HeadedMegaparsec as Parser
import {-# SOURCE #-} PostgresqlSyntax.Ast.AExpr (AExpr, filteredParser)
import PostgresqlSyntax.Ast.AscDesc
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Ast.NullsOrder
import PostgresqlSyntax.Ast.QualAllOp
import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Parser
import PostgresqlSyntax.IsAst
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
  toTextBuilder = \case
    UsingSortBy a b c -> toTextBuilder a <> " USING " <> toTextBuilder b <> suffixMaybe toTextBuilder c
    AscDescSortBy a b c -> toTextBuilder a <> suffixMaybe toTextBuilder b <> suffixMaybe toTextBuilder c
  parser = do
    a <- filteredParser ["using", "asc", "desc", "nulls"]
    asum
      [ do
          Parser.space1
          keyword "using"
          Parser.space1
          Parser.endHead
          b <- parser
          c <- optional (Parser.space1 *> parser)
          return (UsingSortBy a b c),
        do
          b <- optional (Parser.space1 *> parser)
          c <- optional (Parser.space1 *> parser)
          return (AscDescSortBy a b c)
      ]

instance Qc.Arbitrary SortBy where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.oneof
      [ UsingSortBy <$> Qc.scale (`div` 2) Qc.arbitrary <*> Qc.arbitrary <*> Qc.arbitrary,
        AscDescSortBy <$> Qc.scale (`div` 2) Qc.arbitrary <*> Qc.arbitrary <*> Qc.arbitrary
      ]
