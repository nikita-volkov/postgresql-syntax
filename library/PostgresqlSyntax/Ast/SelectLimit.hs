module PostgresqlSyntax.Ast.SelectLimit where

import PostgresqlSyntax.Ast.LimitClause
import PostgresqlSyntax.Ast.OffsetClause
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import qualified Test.QuickCheck as Qc

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
    LimitOffsetSelectLimit a b -> TextBuilders.lexemes [toTextBuilder a, toTextBuilder b]
    OffsetLimitSelectLimit a b -> TextBuilders.lexemes [toTextBuilder a, toTextBuilder b]
    LimitSelectLimit a -> toTextBuilder a
    OffsetSelectLimit a -> toTextBuilder a
  parser =
    asum
      [ do
          a <- parser
          LimitOffsetSelectLimit a <$> (Parsers.space1 *> parser) <|> pure (LimitSelectLimit a),
        do
          a <- parser
          OffsetLimitSelectLimit a <$> (Parsers.space1 *> parser) <|> pure (OffsetSelectLimit a)
      ]

instance Qc.Arbitrary SelectLimit where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.oneof
      [ LimitOffsetSelectLimit <$> Qc.arbitrary <*> Qc.arbitrary,
        OffsetLimitSelectLimit <$> Qc.arbitrary <*> Qc.arbitrary,
        LimitSelectLimit <$> Qc.arbitrary,
        OffsetSelectLimit <$> Qc.arbitrary
      ]
