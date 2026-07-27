module PostgresqlSyntax.Ast.SelectLimit where

import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Ast.LimitClause
import PostgresqlSyntax.Ast.OffsetClause
import PostgresqlSyntax.Extras.HeadedMegaparsec hiding (run)
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)

-- |
-- ==== References
-- @
-- select_limit:
--   | limit_clause offset_clause
--   | offset_clause limit_clause
--   | limit_clause
--   | offset_clause
-- @
data SelectLimit
  = LimitOffsetSelectLimit LimitClause OffsetClause
  | OffsetLimitSelectLimit OffsetClause LimitClause
  | LimitSelectLimit LimitClause
  | OffsetSelectLimit OffsetClause
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst SelectLimit where
  toTextBuilder = \case
    LimitOffsetSelectLimit a b -> lexemes [toTextBuilder a, toTextBuilder b]
    OffsetLimitSelectLimit a b -> lexemes [toTextBuilder a, toTextBuilder b]
    LimitSelectLimit a -> toTextBuilder a
    OffsetSelectLimit a -> toTextBuilder a
  parser =
    asum
      [ do
          a <- parser
          LimitOffsetSelectLimit a <$> (space1 *> parser) <|> pure (LimitSelectLimit a),
        do
          a <- parser
          OffsetLimitSelectLimit a <$> (space1 *> parser) <|> pure (OffsetSelectLimit a)
      ]

instance Arbitrary SelectLimit where
  arbitrary =
    oneof
      [ LimitOffsetSelectLimit <$> arbitrary <*> arbitrary,
        OffsetLimitSelectLimit <$> arbitrary <*> arbitrary,
        LimitSelectLimit <$> arbitrary,
        OffsetSelectLimit <$> arbitrary
      ]
