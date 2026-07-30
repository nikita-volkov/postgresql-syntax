module PostgresqlSyntax.Ast.GenericType where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Ast.Attrs
import PostgresqlSyntax.Ast.ExprList
import PostgresqlSyntax.Ast.Ident
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
import PostgresqlSyntax.IsAst
import qualified PostgresqlSyntax.KeywordSet as KeywordSet
import PostgresqlSyntax.Prelude
import qualified Test.QuickCheck as Qc

-- |
-- ==== References
-- @
-- GenericType:
--   | type_function_name opt_type_modifiers
--   | type_function_name attrs opt_type_modifiers
-- @
--
-- 'PostgresqlSyntax.Ast.TypeFunctionName' is a bare alias to 'Ident', but its
-- /parser/ (kept in "PostgresqlSyntax.Parsing" since @TypeFunctionName@
-- itself isn't extracted in this batch) is more permissive than plain
-- 'Ident'. Since this module sits below "PostgresqlSyntax.Parsing" (no
-- import cycle allowed), that TypeFunctionName-flavored element parser is
-- duplicated here (mirroring @typeFunctionName@'s definition), same as
-- 'PostgresqlSyntax.Ast.NameList'.
data GenericType = GenericType Ident (Maybe Attrs) (Maybe ExprList)
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst GenericType where
  toTextBuilder settings (GenericType a b c) = toTextBuilder settings a <> foldMap (toTextBuilder settings) b <> TextBuilders.suffixMaybe (TextBuilders.renderInParens . toTextBuilder settings) c
  parser settings = do
    a <- typeFunctionNameLikeName
    Parser.endHead
    b <- optional (Parsers.space *> parser settings)
    c <- optional (Parsers.space1 *> Parsers.inParens (parser settings))
    return (GenericType a b c)
    where
      typeFunctionNameLikeName = Parsers.keywordNameFromSet UnquotedIdent KeywordSet.typeFunctionName <|> parser settings

instance Qc.Arbitrary GenericType where
  shrink = Qc.genericShrink
  arbitrary = GenericType <$> arbitrary <*> arbitrary <*> arbitrary
