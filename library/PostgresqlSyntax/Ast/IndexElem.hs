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
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
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
  toTextBuilder (IndexElem a b c d e) =
    toTextBuilder a
      <> TextBuilders.suffixMaybe collate b
      <> TextBuilders.suffixMaybe toTextBuilder c
      <> TextBuilders.suffixMaybe toTextBuilder d
      <> TextBuilders.suffixMaybe toTextBuilder e
    where
      collate = mappend "COLLATE " . toTextBuilder
  parser =
    IndexElem
      <$> (parser <* Parser.endHead)
      <*> optional (Parsers.space1 *> collate)
      <*> optional (Parsers.space1 *> class_)
      <*> optional (Parsers.space1 *> parser)
      <*> optional (Parsers.space1 *> parser)
    where
      collate = Parsers.keyword "collate" *> Parsers.space1 *> Parser.endHead *> parser

      -- gram.y:8557 index_elem: ColId index_elem_options, and gram.y:8524
      -- opt_nulls_order. opt_class is an any_name, i.e. a bare ColId, so
      -- of the words that can terminate it only the unreserved NULLS
      -- (kwlist.h:315) is a genuine hazard — ASC/DESC are reserved
      -- (kwlist.h:47,138) and were never candidates.
      class_ = filteredParser ["nulls"]

instance Qc.Arbitrary IndexElem where
  shrink = Qc.genericShrink
  arbitrary =
    IndexElem
      <$> Qc.arbitrary
      <*> Gens.terminatingMaybe Qc.arbitrary
      <*> Gens.terminatingMaybe Qc.arbitrary
      <*> Qc.arbitrary
      <*> Qc.arbitrary
