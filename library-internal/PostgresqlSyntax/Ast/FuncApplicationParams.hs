module PostgresqlSyntax.Ast.FuncApplicationParams where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Ast.FuncArgExpr
import PostgresqlSyntax.Ast.SortClause
import qualified PostgresqlSyntax.Helpers.Gens as Gens
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
import PostgresqlSyntax.IsAst
import qualified PostgresqlSyntax.Predicate as Predicate
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
  toTextBuilder settings = \case
    NormalFuncApplicationParams a b c ->
      TextBuilders.optLexemes
        [ fmap TextBuilders.renderAllOrDistinct a,
          Just (TextBuilders.commaNonEmpty (toTextBuilder settings) b),
          fmap (toTextBuilder settings) c
        ]
    VariadicFuncApplicationParams a b c ->
      TextBuilders.optLexemes
        [ fmap (flip mappend "," . TextBuilders.commaNonEmpty (toTextBuilder settings)) a,
          Just "VARIADIC",
          Just (toTextBuilder settings b),
          fmap (toTextBuilder settings) c
        ]
    StarFuncApplicationParams -> "*"
  parser settings =
    asum
      [ starFuncApplicationParams,
        listVariadicFuncApplicationParams,
        singleVariadicFuncApplicationParams,
        normalFuncApplicationParams
      ]
    where
      normalFuncApplicationParams = do
        optAllOrDistinct <- optional (Parsers.allOrDistinct <* Parsers.space1)
        argList <- Parsers.sep1 Parsers.commaSeparator (parser settings)
        Parser.endHead
        optSortClause <- optional (Parsers.space1 *> parser settings)
        return (NormalFuncApplicationParams optAllOrDistinct argList optSortClause)
      singleVariadicFuncApplicationParams = do
        Parsers.keyword "variadic"
        Parsers.space1
        Parser.endHead
        arg <- parser settings
        optSortClause <- optional (Parsers.space1 *> parser settings)
        return (VariadicFuncApplicationParams Nothing arg optSortClause)

      -- @func_arg_list ',' VARIADIC func_arg_expr@: one or more
      -- comma-separated 'FuncArgExpr's, where the final comma is
      -- immediately followed by (and the @VARIADIC@ Parsers.keyword itself consumed
      -- by) the terminating branch — equivalent to the pre-extraction
      -- @sepEnd1 Parsers.commaSeparator (Parsers.keyword "variadic" <* space1) funcArgExpr@.
      listVariadicFuncApplicationParams = do
        argList <- Parser.wrapToHead argListEndingInVariadic
        Parser.endHead
        arg <- parser settings
        optSortClause <- optional (Parsers.space1 *> parser settings)
        return (VariadicFuncApplicationParams (Just argList) arg optSortClause)
      argListEndingInVariadic = do
        a <- parser settings
        Parsers.commaSeparator
        asum
          [ pure (a :| []) <* (Parsers.keyword "variadic" *> Parsers.space1),
            (\(b :| bs) -> a :| b : bs) <$> argListEndingInVariadic
          ]
      -- A bare '*' char can also be the leading char of a longer operator
      -- token (e.g. "*#" in @foo(*# DEFAULT)@'s @PrefixQualOpAExpr@), so
      -- this only commits to the wildcard reading when no further op char
      -- follows — otherwise it falls through to 'normalFuncApplicationParams',
      -- which parses the '*' as the start of that operator instead.
      starFuncApplicationParams =
        Parsers.space
          *> Parsers.char '*'
          *> Parsers.notFollowedBy (Parsers.satisfy Predicate.opChar)
          *> Parser.endHead
          *> Parsers.space
            $> StarFuncApplicationParams

instance Qc.Arbitrary FuncApplicationParams where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.oneof
      [ NormalFuncApplicationParams <$> Qc.arbitrary <*> nonEmptyOf 8 <*> Gens.terminatingMaybe Qc.arbitrary,
        VariadicFuncApplicationParams <$> maybeNonEmptyOf 8 <*> Qc.arbitrary <*> Gens.terminatingMaybe Qc.arbitrary,
        pure StarFuncApplicationParams
      ]
    where
      nonEmptyOf hi = Gens.nonEmptyUpTo (hi - 1) Qc.arbitrary
      maybeNonEmptyOf hi = Qc.oneof [pure Nothing, Just <$> nonEmptyOf hi]
