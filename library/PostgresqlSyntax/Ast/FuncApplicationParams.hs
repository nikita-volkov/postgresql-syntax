module PostgresqlSyntax.Ast.FuncApplicationParams where

import HeadedMegaparsec
import PostgresqlSyntax.Ast.FuncArgExpr
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Ast.SortClause
import PostgresqlSyntax.Extras.HeadedMegaparsec hiding (run)
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import Test.QuickCheck (scale)

-- |
-- ==== References
-- @
-- func_application:
--   |  func_name '(' ')'
--   |  func_name '(' func_arg_list opt_sort_clause ')'
--   |  func_name '(' VARIADIC func_arg_expr opt_sort_clause ')'
--   |  func_name '(' func_arg_list ',' VARIADIC func_arg_expr opt_sort_clause ')'
--   |  func_name '(' ALL func_arg_list opt_sort_clause ')'
--   |  func_name '(' DISTINCT func_arg_list opt_sort_clause ')'
--   |  func_name '(' '*' ')'
-- @
data FuncApplicationParams
  = NormalFuncApplicationParams (Maybe Bool) (NonEmpty FuncArgExpr) (Maybe SortClause)
  | VariadicFuncApplicationParams (Maybe (NonEmpty FuncArgExpr)) FuncArgExpr (Maybe SortClause)
  | StarFuncApplicationParams
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst FuncApplicationParams where
  toTextBuilder = \case
    NormalFuncApplicationParams a b c ->
      optLexemes
        [ fmap renderAllOrDistinct a,
          Just (commaNonEmpty toTextBuilder b),
          fmap toTextBuilder c
        ]
    VariadicFuncApplicationParams a b c ->
      optLexemes
        [ fmap (flip mappend "," . commaNonEmpty toTextBuilder) a,
          Just "VARIADIC",
          Just (toTextBuilder b),
          fmap toTextBuilder c
        ]
    StarFuncApplicationParams -> "*"
  parser =
    asum
      [ starFuncApplicationParams,
        listVariadicFuncApplicationParams,
        singleVariadicFuncApplicationParams,
        normalFuncApplicationParams
      ]
    where
      normalFuncApplicationParams = do
        optAllOrDistinct <- optional (allOrDistinct <* space1)
        argList <- sep1 commaSeparator parser
        endHead
        optSortClause <- optional (space1 *> parser)
        return (NormalFuncApplicationParams optAllOrDistinct argList optSortClause)
      singleVariadicFuncApplicationParams = do
        keyword "variadic"
        space1
        endHead
        arg <- parser
        optSortClause <- optional (space1 *> parser)
        return (VariadicFuncApplicationParams Nothing arg optSortClause)
      -- |
      -- @func_arg_list ',' VARIADIC func_arg_expr@: one or more
      -- comma-separated 'FuncArgExpr's, where the final comma is
      -- immediately followed by (and the @VARIADIC@ keyword itself consumed
      -- by) the terminating branch — equivalent to the pre-extraction
      -- @sepEnd1 commaSeparator (keyword "variadic" <* space1) funcArgExpr@.
      listVariadicFuncApplicationParams = do
        argList <- wrapToHead argListEndingInVariadic
        endHead
        arg <- parser
        optSortClause <- optional (space1 *> parser)
        return (VariadicFuncApplicationParams (Just argList) arg optSortClause)
      argListEndingInVariadic = do
        a <- parser
        commaSeparator
        asum
          [ pure (a :| []) <* (keyword "variadic" *> space1),
            (\(b :| bs) -> a :| b : bs) <$> argListEndingInVariadic
          ]
      starFuncApplicationParams = space *> char '*' *> endHead *> space $> StarFuncApplicationParams

instance Arbitrary FuncApplicationParams where
  arbitrary =
    oneof
      [ NormalFuncApplicationParams <$> arbitrary <*> nonEmptyOf 8 <*> scale (`div` 2) arbitrary,
        VariadicFuncApplicationParams <$> maybeNonEmptyOf 8 <*> scale (`div` 2) arbitrary <*> scale (`div` 2) arbitrary,
        pure StarFuncApplicationParams
      ]
    where
      nonEmptyOf hi = do
        len <- choose (0, hi - 1)
        x <- scale (`div` 2) arbitrary
        xs <- vectorOf len (scale (`div` 2) arbitrary)
        pure (x :| xs)
      maybeNonEmptyOf hi = oneof [pure Nothing, Just <$> nonEmptyOf hi]
