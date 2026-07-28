module PostgresqlSyntax.Ast.FuncApplicationParams where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Ast.FuncArgExpr
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Ast.SortClause
import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Parser
import qualified PostgresqlSyntax.Extras.QuickCheck as Qc
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import qualified Test.QuickCheck as Qc

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
        optAllOrDistinct <- optional (allOrDistinct <* Parser.space1)
        argList <- Parser.sep1 commaSeparator parser
        Parser.endHead
        optSortClause <- optional (Parser.space1 *> parser)
        return (NormalFuncApplicationParams optAllOrDistinct argList optSortClause)
      singleVariadicFuncApplicationParams = do
        keyword "variadic"
        Parser.space1
        Parser.endHead
        arg <- parser
        optSortClause <- optional (Parser.space1 *> parser)
        return (VariadicFuncApplicationParams Nothing arg optSortClause)

      -- @func_arg_list ',' VARIADIC func_arg_expr@: one or more
      -- comma-separated 'FuncArgExpr's, where the final comma is
      -- immediately followed by (and the @VARIADIC@ keyword itself consumed
      -- by) the terminating branch — equivalent to the pre-extraction
      -- @sepEnd1 commaSeparator (keyword "variadic" <* space1) funcArgExpr@.
      listVariadicFuncApplicationParams = do
        argList <- Parser.wrapToHead argListEndingInVariadic
        Parser.endHead
        arg <- parser
        optSortClause <- optional (Parser.space1 *> parser)
        return (VariadicFuncApplicationParams (Just argList) arg optSortClause)
      argListEndingInVariadic = do
        a <- parser
        commaSeparator
        asum
          [ pure (a :| []) <* (keyword "variadic" *> Parser.space1),
            (\(b :| bs) -> a :| b : bs) <$> argListEndingInVariadic
          ]
      starFuncApplicationParams = Parser.space *> Parser.char '*' *> Parser.endHead *> Parser.space $> StarFuncApplicationParams

instance Qc.Arbitrary FuncApplicationParams where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.oneof
      [ NormalFuncApplicationParams <$> Qc.arbitrary <*> nonEmptyOf 8 <*> Qc.scale (`div` 2) Qc.arbitrary,
        VariadicFuncApplicationParams <$> maybeNonEmptyOf 8 <*> Qc.scale (`div` 2) Qc.arbitrary <*> Qc.scale (`div` 2) Qc.arbitrary,
        pure StarFuncApplicationParams
      ]
    where
      nonEmptyOf hi = Qc.nonEmptyUpTo (hi - 1) Qc.arbitrary
      maybeNonEmptyOf hi = Qc.oneof [pure Nothing, Just <$> nonEmptyOf hi]
