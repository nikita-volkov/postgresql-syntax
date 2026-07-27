module PostgresqlSyntax.Ast.AnyOperator where

import HeadedMegaparsec hiding (string)
import PostgresqlSyntax.Ast.AllOp
import PostgresqlSyntax.Ast.Ident
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Extras.HeadedMegaparsec hiding (run)
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, option, some, try)
import qualified PostgresqlSyntax.KeywordSet as KeywordSet
import Test.QuickCheck (scale)

-- |
-- ==== References
-- @
-- any_operator:
--   | all_Op
--   | ColId '.' any_operator
-- @
--
-- 'PostgresqlSyntax.Ast.ColId' is a bare alias to 'Ident', but its /parser/
-- (kept in "PostgresqlSyntax.Parsing" since @ColId@ itself isn't extracted
-- in this batch) is more permissive than plain 'Ident'. Since this module
-- sits below "PostgresqlSyntax.Parsing" (no import cycle allowed), that
-- ColId-flavored element parser is duplicated here (mirroring @colId@'s
-- definition), same as 'PostgresqlSyntax.Ast.NameList'.
data AnyOperator
  = AllOpAnyOperator AllOp
  | QualifiedAnyOperator Ident AnyOperator
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst AnyOperator where
  toTextBuilder = \case
    AllOpAnyOperator a -> toTextBuilder a
    QualifiedAnyOperator a b -> toTextBuilder a <> "." <> toTextBuilder b
  parser =
    asum
      [ AllOpAnyOperator <$> parser,
        QualifiedAnyOperator <$> colIdLikeName <*> (space *> char '.' *> space *> parser)
      ]
    where
      colIdLikeName =
        label "identifier"
          $ parser
          <|> keywordNameFromSet UnquotedIdent (KeywordSet.unreservedKeyword <> KeywordSet.colNameKeyword)

instance Arbitrary AnyOperator where
  arbitrary = sized $ \n ->
    if n <= 1
      then AllOpAnyOperator <$> arbitrary
      else
        oneof
          [ AllOpAnyOperator <$> arbitrary,
            QualifiedAnyOperator <$> arbitrary <*> scale (`div` 2) arbitrary
          ]
