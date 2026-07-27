module PostgresqlSyntax.Ast.SubqueryOp where

import HeadedMegaparsec hiding (string)
import PostgresqlSyntax.Ast.AllOp
import PostgresqlSyntax.Ast.AnyOperator
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Extras.HeadedMegaparsec hiding (run)
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, option, some, try)

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
    AnySubqueryOp a -> "OPERATOR " <> renderInParens (toTextBuilder a)
    LikeSubqueryOp a -> bool "" "NOT " a <> "LIKE"
    IlikeSubqueryOp a -> bool "" "NOT " a <> "ILIKE"
  parser =
    asum
      [ AnySubqueryOp <$> (keyword "operator" *> space *> endHead *> inParens parser),
        do
          a <- trueIfPresent (keyword "not" *> space1)
          LikeSubqueryOp a <$ keyword "like" <|> IlikeSubqueryOp a <$ keyword "ilike",
        AllSubqueryOp <$> parser
      ]

instance Arbitrary SubqueryOp where
  arbitrary =
    oneof
      [ AllSubqueryOp <$> arbitrary,
        AnySubqueryOp <$> arbitrary,
        LikeSubqueryOp <$> arbitrary,
        IlikeSubqueryOp <$> arbitrary
      ]
