module PostgresqlSyntax.Ast.SortBy where

import qualified HeadedMegaparsec as Parser
import {-# SOURCE #-} PostgresqlSyntax.Ast.AExpr (AExpr, filteredParser)
import PostgresqlSyntax.Ast.AscDesc
import PostgresqlSyntax.Ast.NullsOrder
import PostgresqlSyntax.Ast.QualAllOp
import qualified PostgresqlSyntax.Helpers.Gens as Gens
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
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
    UsingSortBy a b c -> toTextBuilder a <> " USING " <> toTextBuilder b <> TextBuilders.suffixMaybe toTextBuilder c
    AscDescSortBy a b c -> toTextBuilder a <> TextBuilders.suffixMaybe toTextBuilder b <> TextBuilders.suffixMaybe toTextBuilder c
  parser = do
    -- gram.y:14056 sortby. Of the four words that can terminate this
    -- a_expr, only NULLS is unreserved (kwlist.h:315) and therefore a
    -- legal ColId; USING/ASC/DESC are reserved (kwlist.h:496,47,138) and
    -- can never be absorbed. Postgres disambiguates NULLS with a
    -- two-token lexer lookahead (NULLS_LA, gram.y:864); this exclusion is
    -- the coarser recursive-descent equivalent.
    a <- filteredParser ["nulls"]
    asum
      [ do
          Parsers.space1
          Parsers.keyword "using"
          Parsers.space1
          Parser.endHead
          b <- parser
          c <- optional (Parsers.space1 *> parser)
          return (UsingSortBy a b c),
        do
          b <- optional (Parsers.space1 *> parser)
          c <- optional (Parsers.space1 *> parser)
          return (AscDescSortBy a b c)
      ]

instance Qc.Arbitrary SortBy where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.oneof
      [ UsingSortBy <$> Gens.downscale Qc.arbitrary <*> Qc.arbitrary <*> Qc.arbitrary,
        AscDescSortBy <$> Gens.downscale Qc.arbitrary <*> Qc.arbitrary <*> Qc.arbitrary
      ]
