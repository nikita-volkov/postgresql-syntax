module PostgresqlSyntax.Ast.Attrs where

import Control.Applicative.Combinators.NonEmpty (some)
import qualified Data.List.NonEmpty as NonEmpty
import qualified HeadedMegaparsec as Parser
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
  toTextBuilder (Attrs a) = foldMap (mappend "." . toTextBuilder) a
  parser = Attrs <$> some (Parser.char '.' *> Parser.endHead *> Parser.space *> colLabelLikeName)
    where
      colLabelLikeName =
        Parser.label "column label"
          $ keywordNameFromSet UnquotedIdent KeywordSet.keyword
          <|> parser

instance Qc.Arbitrary Attrs where
  arbitrary = do
    len <- Qc.choose (1, 10)
    Attrs . NonEmpty.fromList <$> Qc.vectorOf len Qc.arbitrary
