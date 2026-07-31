module PostgresqlSyntax.Ast.AnyName where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Algebra
import PostgresqlSyntax.Ast.Attrs
import PostgresqlSyntax.Ast.Ident
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.KeywordSet as KeywordSet
import PostgresqlSyntax.Prelude
import qualified Test.QuickCheck as Qc

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
  toTextBuilder settings (AnyName a b) = toTextBuilder settings a <> foldMap (toTextBuilder settings) b
  parser settings = AnyName <$> (Parser.wrapToHead colIdLikeName <* Parser.endHead) <*> optional (Parsers.space *> parser settings)
    where
      colIdLikeName =
        Parser.label "identifier" $
          parser settings
            <|> Parsers.keywordNameFromSet UnquotedIdent (KeywordSet.unreservedKeyword <> KeywordSet.colNameKeyword)

instance Qc.Arbitrary AnyName where
  shrink = Qc.genericShrink
  arbitrary = AnyName <$> arbitrary <*> arbitrary
