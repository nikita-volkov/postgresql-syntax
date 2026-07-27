module PostgresqlSyntax.Ast.AnyName where

import HeadedMegaparsec
import PostgresqlSyntax.Ast.Attrs
import PostgresqlSyntax.Ast.Ident
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Extras.HeadedMegaparsec hiding (run)
import PostgresqlSyntax.IsAst
import qualified PostgresqlSyntax.KeywordSet as KeywordSet
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)

-- |
-- ==== References
-- @
-- any_name:
--   | ColId
--   | ColId attrs
-- @
--
-- 'PostgresqlSyntax.Ast.ColId' is a bare alias to 'Ident', but its /parser/
-- (kept in "PostgresqlSyntax.Parsing" since @ColId@ itself isn't extracted
-- in this batch) is more permissive than plain 'Ident'. Since this module
-- sits below "PostgresqlSyntax.Parsing" (no import cycle allowed), that
-- ColId-flavored element parser is duplicated here (mirroring @colId@'s
-- definition), same as 'PostgresqlSyntax.Ast.NameList'. (This is also
-- exactly what "PostgresqlSyntax.Parsing"'s @customizedAnyName@ does with
-- its @colId@ argument; that generic helper stays in "PostgresqlSyntax.Parsing"
-- since its other caller, @filteredAnyName@, passes a filtered variant.)
data AnyName = AnyName Ident (Maybe Attrs)
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst AnyName where
  toTextBuilder (AnyName a b) = toTextBuilder a <> foldMap toTextBuilder b
  parser = AnyName <$> (wrapToHead colIdLikeName <* endHead) <*> optional (space *> parser)
    where
      colIdLikeName =
        label "identifier"
          $ parser
          <|> keywordNameFromSet UnquotedIdent (KeywordSet.unreservedKeyword <> KeywordSet.colNameKeyword)

instance Arbitrary AnyName where
  arbitrary = AnyName <$> arbitrary <*> arbitrary
