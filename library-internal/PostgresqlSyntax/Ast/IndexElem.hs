module PostgresqlSyntax.Ast.IndexElem where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Ast.AnyName
import PostgresqlSyntax.Ast.AscDesc
import PostgresqlSyntax.Ast.IndexElemDef
import PostgresqlSyntax.Ast.NullsOrder
import qualified PostgresqlSyntax.Helpers.Gens as Gens
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude
import qualified Test.QuickCheck as Qc

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
  toTextBuilder settings (IndexElem a b c d e) =
    toTextBuilder settings a
      <> TextBuilders.suffixMaybe collate b
      <> TextBuilders.suffixMaybe (toTextBuilder settings) c
      <> TextBuilders.suffixMaybe (toTextBuilder settings) d
      <> TextBuilders.suffixMaybe (toTextBuilder settings) e
    where
      collate = mappend "COLLATE " . toTextBuilder settings
  parser settings =
    IndexElem
      <$> (parser settings <* Parser.endHead)
      <*> optional (Parsers.space1 *> collate)
      <*> optional (Parsers.space1 *> class_)
      <*> optional (Parsers.space1 *> parser settings)
      <*> optional (Parsers.space1 *> parser settings)
    where
      collate = Parsers.keyword "collate" *> Parsers.space1 *> Parser.endHead *> parser settings

      -- gram.y:8558 index_elem: ColId index_elem_options, and gram.y:8596
      -- opt_nulls_order (index_elem_options inlines opt_qualified_name at
      -- gram.y:8525 for the operator-class name). That name is a bare
      -- ColId, so of the words that can terminate it only the unreserved
      -- NULLS (kwlist.h:315) is a genuine hazard — ASC/DESC are reserved
      -- (kwlist.h:47,138) and were never candidates.
      class_ = filteredParser settings ["nulls"]

instance Qc.Arbitrary IndexElem where
  shrink = Qc.genericShrink
  arbitrary =
    IndexElem
      <$> Qc.arbitrary
      <*> Gens.terminatingMaybe Qc.arbitrary
      <*> Gens.terminatingMaybe Qc.arbitrary
      <*> Qc.arbitrary
      <*> Qc.arbitrary
