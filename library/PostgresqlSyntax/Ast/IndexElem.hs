module PostgresqlSyntax.Ast.IndexElem where

import PostgresqlSyntax.Ast.AnyName
import PostgresqlSyntax.Ast.AscDesc
import HeadedMegaparsec
import PostgresqlSyntax.Ast.IndexElemDef
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Ast.NullsOrder
import PostgresqlSyntax.Extras.HeadedMegaparsec hiding (run)
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import Test.QuickCheck (scale)

-- |
-- ==== References
-- @
-- index_elem:
--   | ColId opt_collate opt_class opt_asc_desc opt_nulls_order
--   | func_expr_windowless opt_collate opt_class opt_asc_desc opt_nulls_order
--   | '(' a_expr ')' opt_collate opt_class opt_asc_desc opt_nulls_order
-- @
--
-- @opt_collate@\/@opt_class@ are bare aliases to
-- 'PostgresqlSyntax.Ast.AnyName'.
data IndexElem = IndexElem IndexElemDef (Maybe AnyName) (Maybe AnyName) (Maybe AscDesc) (Maybe NullsOrder)
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst IndexElem where
  toTextBuilder (IndexElem a b c d e) =
    toTextBuilder a
      <> suffixMaybe collate b
      <> suffixMaybe toTextBuilder c
      <> suffixMaybe toTextBuilder d
      <> suffixMaybe toTextBuilder e
    where
      collate = mappend "COLLATE " . toTextBuilder
  parser =
    IndexElem
      <$> (parser <* endHead)
      <*> optional (space1 *> collate)
      <*> optional (space1 *> class_)
      <*> optional (space1 *> parser)
      <*> optional (space1 *> parser)
    where
      collate = keyword "collate" *> space1 *> endHead *> parser
      -- |
      -- Duplicated 'PostgresqlSyntax.Ast.AnyName.filteredParser' call,
      -- mirroring the pre-extraction @class_ = filteredAnyName ["asc",
      -- "desc", "nulls"]@ — excludes the words that terminate this
      -- position.
      class_ = filteredParser ["asc", "desc", "nulls"]

instance Arbitrary IndexElem where
  arbitrary =
    IndexElem
      <$> scale (`div` 2) arbitrary
      <*> scale (`div` 2) arbitrary
      <*> scale (`div` 2) arbitrary
      <*> arbitrary
      <*> arbitrary
