module PostgresqlSyntax.Ast.AnyName where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Ast.Attrs
import PostgresqlSyntax.Ast.Ident
import PostgresqlSyntax.Ast.Internal
import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Parser
import PostgresqlSyntax.IsAst
import qualified PostgresqlSyntax.KeywordSet as KeywordSet
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
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
  toTextBuilder (AnyName a b) = toTextBuilder a <> foldMap toTextBuilder b
  parser = AnyName <$> (Parser.wrapToHead colIdLikeName <* Parser.endHead) <*> optional (Parser.space *> parser)
    where
      colIdLikeName =
        Parser.label "identifier" $
          parser
            <|> keywordNameFromSet UnquotedIdent (KeywordSet.unreservedKeyword <> KeywordSet.colNameKeyword)

instance Qc.Arbitrary AnyName where
  shrink = Qc.genericShrink
  arbitrary = AnyName <$> arbitrary <*> arbitrary

-- | 'parser', but rejecting the given words when they'd otherwise be
-- accepted as the leading identifier — needed by
-- "PostgresqlSyntax.Ast.IndexElem"'s @opt_class@ position, mirroring the
-- pre-extraction @filteredAnyName@.
filteredParser :: [Text] -> Parser AnyName
filteredParser excluded = AnyName <$> (Parser.wrapToHead (filteredColIdLike UnquotedIdent parser excluded) <* Parser.endHead) <*> optional (Parser.space *> parser)
