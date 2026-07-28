module PostgresqlSyntax.Ast.AnyOperator where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Ast.AllOp
import PostgresqlSyntax.Ast.Ident
import qualified PostgresqlSyntax.Helpers.Gens as Gens
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import PostgresqlSyntax.IsAst
import qualified PostgresqlSyntax.KeywordSet as KeywordSet
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import qualified Test.QuickCheck as Qc

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
        QualifiedAnyOperator <$> colIdLikeName <*> (Parsers.space *> Parsers.char '.' *> Parsers.space *> parser)
      ]
    where
      colIdLikeName =
        Parser.label "identifier" $
          parser
            <|> Parsers.keywordNameFromSet UnquotedIdent (KeywordSet.unreservedKeyword <> KeywordSet.colNameKeyword)

instance Qc.Arbitrary AnyOperator where
  shrink = Qc.genericShrink
  arbitrary = Qc.sized $ \n ->
    if n <= 1
      then AllOpAnyOperator <$> Qc.arbitrary
      else
        Qc.oneof
          [ AllOpAnyOperator <$> Qc.arbitrary,
            QualifiedAnyOperator <$> Qc.arbitrary <*> Gens.downscale Qc.arbitrary
          ]
