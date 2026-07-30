module PostgresqlSyntax.Ast.Attrs where

import qualified Control.Applicative.Combinators.NonEmpty as NonEmpty
import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Ast.Ident
import qualified PostgresqlSyntax.Helpers.Gens as Gens
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import PostgresqlSyntax.IsAst
import qualified PostgresqlSyntax.KeywordSet as KeywordSet
import PostgresqlSyntax.Prelude
import qualified Test.QuickCheck as Qc

-- |
-- ==== References
-- @
-- attrs:
--   | '.' attr_name
--   | attrs '.' attr_name
-- @
--
-- 'PostgresqlSyntax.Ast.AttrName' is a bare alias to
-- 'PostgresqlSyntax.Ast.ColLabel' which is a bare alias to 'Ident', but the
-- @ColLabel@ /parser/ (kept in "PostgresqlSyntax.Parsing" since @ColLabel@
-- itself isn't extracted in this batch) is more permissive than plain
-- 'Ident': it additionally accepts the full @keyword@ lexical class as an
-- identifier. Since this module sits below "PostgresqlSyntax.Parsing" (no
-- import cycle allowed), that ColLabel-flavored element parser is
-- duplicated here (mirroring @colLabel@'s definition) rather than reused,
-- to preserve exact parsing behavior. (See 'PostgresqlSyntax.Ast.NameList'
-- for the same pattern applied to @ColId@.)
newtype Attrs = Attrs (NonEmpty Ident)
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst Attrs where
  toTextBuilder settings (Attrs a) = foldMap (mappend "." . toTextBuilder settings) a
  parser settings = Attrs <$> NonEmpty.some (Parsers.char '.' *> Parser.endHead *> Parsers.space *> colLabelLikeName)
    where
      colLabelLikeName =
        Parser.label "column label" $
          Parsers.keywordNameFromSet UnquotedIdent KeywordSet.keyword
            <|> parser settings

instance Qc.Arbitrary Attrs where
  shrink = Qc.genericShrink
  arbitrary = Attrs <$> Gens.nonEmptyUpTo 9 Qc.arbitrary
