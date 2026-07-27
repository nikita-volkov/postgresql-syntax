module PostgresqlSyntax.Ast.NameList where

import HeadedMegaparsec hiding (string)
import PostgresqlSyntax.Ast.Ident
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Extras.HeadedMegaparsec hiding (run)
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, option, some, try)
import qualified PostgresqlSyntax.KeywordSet as KeywordSet

-- |
-- ==== References
-- @
-- name_list:
--   | name
--   | name_list ',' name
-- @
--
-- 'Name' is a bare alias to 'PostgresqlSyntax.Ast.ColId' which is a bare
-- alias to 'Ident', but the @ColId@ /parser/ (kept in "PostgresqlSyntax.Parsing"
-- since @ColId@ itself isn't extracted in this batch) is more permissive
-- than plain 'Ident': it additionally accepts the @unreserved_keyword@ and
-- @col_name_keyword@ lexical classes as identifiers. Since this module sits
-- below "PostgresqlSyntax.Parsing" (no import cycle allowed), that
-- ColId-flavored element parser is duplicated here (mirroring @colId@'s
-- definition) rather than reused, to preserve exact parsing behavior.
newtype NameList = NameList (NonEmpty Ident)
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst NameList where
  toTextBuilder (NameList a) = commaNonEmpty toTextBuilder a
  parser = NameList <$> sep1 commaSeparator colIdLikeName
    where
      colIdLikeName =
        label "identifier"
          $ parser
          <|> keywordNameFromSet UnquotedIdent (KeywordSet.unreservedKeyword <> KeywordSet.colNameKeyword)

instance Arbitrary NameList where
  arbitrary = do
    len <- choose (0, 7)
    x <- arbitrary
    xs <- vectorOf len arbitrary
    pure (NameList (x :| xs))
