module PostgresqlSyntax.Ast.IndexElem where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Ast.AnyName
import PostgresqlSyntax.Ast.AscDesc
import PostgresqlSyntax.Ast.IndexElemDef
import PostgresqlSyntax.Ast.NullsOrder
import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Parser
import PostgresqlSyntax.Helpers.Parsers
import PostgresqlSyntax.Helpers.TextBuilders
import qualified PostgresqlSyntax.Extras.QuickCheck as Qc
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
      <> suffixMaybe collate b
      <> suffixMaybe toTextBuilder c
      <> suffixMaybe toTextBuilder d
      <> suffixMaybe toTextBuilder e
    where
      collate = mappend "COLLATE " . toTextBuilder
  parser =
    IndexElem
      <$> (parser <* Parser.endHead)
      <*> optional (Parser.space1 *> collate)
      <*> optional (Parser.space1 *> class_)
      <*> optional (Parser.space1 *> parser)
      <*> optional (Parser.space1 *> parser)
    where
      collate = keyword "collate" *> Parser.space1 *> Parser.endHead *> parser

      -- Duplicated 'PostgresqlSyntax.Ast.AnyName.filteredParser' call,
      -- mirroring the pre-extraction @class_ = filteredAnyName ["asc",
      -- "desc", "nulls"]@ — excludes the words that terminate this
      -- position.
      class_ = filteredParser ["asc", "desc", "nulls"]

instance Qc.Arbitrary IndexElem where
  shrink = Qc.genericShrink
  arbitrary =
    IndexElem
      <$> Qc.scale (`div` 2) Qc.arbitrary
      <*> Qc.terminatingMaybe Qc.arbitrary
      <*> Qc.terminatingMaybe Qc.arbitrary
      <*> Qc.arbitrary
      <*> Qc.arbitrary
