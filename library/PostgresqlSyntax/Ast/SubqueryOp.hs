module PostgresqlSyntax.Ast.SubqueryOp where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Ast.AllOp
import PostgresqlSyntax.Ast.AnyOperator
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import qualified Test.QuickCheck as Qc

-- |
-- ==== References
-- @
-- subquery_Op:
--   | all_Op
--   | OPERATOR '(' any_operator ')'
--   | LIKE
--   | NOT_LA LIKE
--   | ILIKE
--   | NOT_LA ILIKE
-- @
data SubqueryOp
  = AllSubqueryOp AllOp
  | AnySubqueryOp AnyOperator
  | LikeSubqueryOp Bool
  | IlikeSubqueryOp Bool
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst SubqueryOp where
  toTextBuilder = \case
    AllSubqueryOp a -> toTextBuilder a
    AnySubqueryOp a -> "OPERATOR " <> TextBuilders.renderInParens (toTextBuilder a)
    LikeSubqueryOp a -> bool "" "NOT " a <> "LIKE"
    IlikeSubqueryOp a -> bool "" "NOT " a <> "ILIKE"
  parser =
    asum
      [ AnySubqueryOp <$> (Parsers.keyword "operator" *> Parsers.space *> Parser.endHead *> Parsers.inParens parser),
        do
          a <- Parsers.trueIfPresent (Parsers.keyword "not" *> Parsers.space1)
          LikeSubqueryOp a <$ Parsers.keyword "like" <|> IlikeSubqueryOp a <$ Parsers.keyword "ilike",
        AllSubqueryOp <$> parser
      ]

instance Qc.Arbitrary SubqueryOp where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.oneof
      [ AllSubqueryOp <$> Qc.arbitrary,
        AnySubqueryOp <$> Qc.arbitrary,
        LikeSubqueryOp <$> Qc.arbitrary,
        IlikeSubqueryOp <$> Qc.arbitrary
      ]
