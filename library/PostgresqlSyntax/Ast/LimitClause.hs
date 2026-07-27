module PostgresqlSyntax.Ast.LimitClause where

import HeadedMegaparsec
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Ast.SelectFetchFirstValue
import PostgresqlSyntax.Ast.SelectLimitValue
import {-# SOURCE #-} PostgresqlSyntax.Ast.AExpr (AExpr)
import PostgresqlSyntax.Extras.HeadedMegaparsec hiding (run)
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import Test.QuickCheck (scale)

-- |
-- ==== References
-- @
-- limit_clause:
--   | LIMIT select_limit_value
--   | LIMIT select_limit_value ',' select_offset_value
--   | FETCH first_or_next select_fetch_first_value row_or_rows ONLY
--   | FETCH first_or_next row_or_rows ONLY
-- select_offset_value:
--   | a_expr
-- first_or_next:
--   | FIRST_P
--   | NEXT
-- row_or_rows:
--   | ROW
--   | ROWS
-- @
data LimitClause
  = LimitLimitClause SelectLimitValue (Maybe AExpr)
  | FetchOnlyLimitClause Bool (Maybe SelectFetchFirstValue) Bool
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst LimitClause where
  toTextBuilder = \case
    LimitLimitClause a b -> "LIMIT " <> toTextBuilder a <> foldMap (mappend ", " . toTextBuilder) b
    FetchOnlyLimitClause a b c ->
      optLexemes
        [ Just "FETCH",
          Just (firstOrNext a),
          fmap toTextBuilder b,
          Just (rowOrRows c),
          Just "ONLY"
        ]
    where
      firstOrNext = bool "FIRST" "NEXT"
      rowOrRows = bool "ROW" "ROWS"
  parser =
    ( do
        keyword "limit"
        endHead
        space1
        a <- parser
        b <- optional $ do
          commaSeparator
          parser
        return (LimitLimitClause a b)
    )
      <|> ( do
              keyword "fetch"
              endHead
              space1
              a <- firstOrNext
              space1
              asum
                [ do
                    b <- rowOrRows
                    space1
                    keyword "only"
                    return (FetchOnlyLimitClause a Nothing b),
                  do
                    b <- parser
                    space1
                    c <- rowOrRows
                    space1
                    keyword "only"
                    return (FetchOnlyLimitClause a (Just b) c)
                ]
          )
    where
      firstOrNext =
        False <$ keyword "first"
          <|> True <$ keyword "next"
      rowOrRows =
        True <$ keyword "rows"
          <|> False <$ keyword "row"

instance Arbitrary LimitClause where
  arbitrary =
    oneof
      [ LimitLimitClause <$> arbitrary <*> scale (`div` 2) arbitrary,
        FetchOnlyLimitClause <$> arbitrary <*> scale (`div` 2) arbitrary <*> arbitrary
      ]
