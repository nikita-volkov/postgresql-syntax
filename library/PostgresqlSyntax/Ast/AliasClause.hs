module PostgresqlSyntax.Ast.AliasClause where

import HeadedMegaparsec hiding (string)
import PostgresqlSyntax.Ast.Ident
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Ast.NameList
import PostgresqlSyntax.Extras.HeadedMegaparsec hiding (run)
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, option, some, try)
import qualified PostgresqlSyntax.KeywordSet as KeywordSet

-- |
-- ==== References
-- @
-- alias_clause:
--   |  AS ColId '(' name_list ')'
--   |  AS ColId
--   |  ColId '(' name_list ')'
--   |  ColId
-- @
--
-- 'PostgresqlSyntax.Ast.ColId' is a bare alias to 'Ident', but its /parser/
-- (kept in "PostgresqlSyntax.Parsing" since @ColId@ itself isn't extracted
-- in this batch) is more permissive than plain 'Ident'. Since this module
-- sits below "PostgresqlSyntax.Parsing" (no import cycle allowed), that
-- ColId-flavored element parser is duplicated here (mirroring @colId@'s
-- definition), same as 'PostgresqlSyntax.Ast.NameList'.
data AliasClause = AliasClause Bool Ident (Maybe NameList)
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst AliasClause where
  toTextBuilder (AliasClause a b c) =
    optLexemes
      [ if a then Just "AS" else Nothing,
        Just (toTextBuilder b),
        fmap (renderInParens . toTextBuilder) c
      ]
  parser = do
    (as, alias) <- (True,) <$> (keyword "as" *> space1 *> endHead *> colIdLikeName) <|> (False,) <$> colIdLikeName
    columnAliases <- optional (space1 *> inParens parser)
    return (AliasClause as alias columnAliases)
    where
      colIdLikeName =
        label "identifier"
          $ parser
          <|> keywordNameFromSet UnquotedIdent (KeywordSet.unreservedKeyword <> KeywordSet.colNameKeyword)

instance Arbitrary AliasClause where
  arbitrary = AliasClause <$> arbitrary <*> arbitrary <*> arbitrary
