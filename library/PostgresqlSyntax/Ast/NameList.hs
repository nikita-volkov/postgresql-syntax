module PostgresqlSyntax.Ast.NameList where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Ast.Ident
import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Parser
import qualified PostgresqlSyntax.Extras.QuickCheck as Qc
import PostgresqlSyntax.Helpers.Parsers
import PostgresqlSyntax.Helpers.TextBuilders
import PostgresqlSyntax.IsAst
import qualified PostgresqlSyntax.KeywordSet as KeywordSet
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import qualified Test.QuickCheck as Qc

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
  parser = NameList <$> Parser.sep1 commaSeparator colIdLikeName
    where
      colIdLikeName =
        Parser.label "identifier" $
          parser
            <|> keywordNameFromSet UnquotedIdent (KeywordSet.unreservedKeyword <> KeywordSet.colNameKeyword)

instance Qc.Arbitrary NameList where
  shrink = Qc.genericShrink
  arbitrary = NameList <$> Qc.nonEmptyUpTo 7 Qc.arbitrary
