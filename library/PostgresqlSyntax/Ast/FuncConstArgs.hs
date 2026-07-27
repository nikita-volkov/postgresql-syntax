module PostgresqlSyntax.Ast.FuncConstArgs where

import PostgresqlSyntax.Ast.FuncArgExpr
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Ast.SortClause
import PostgresqlSyntax.Extras.HeadedMegaparsec hiding (run)
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import Test.QuickCheck (scale)

-- |
-- The parenthesized-argument-list part of a @func_name '(' func_arg_list
-- opt_sort_clause ')' Sconst@ 'PostgresqlSyntax.Ast.AexprConst' — rendered\/
-- parsed without its enclosing parens, which belong to the caller.
--
-- ==== References
-- @
--   |  func_name '(' func_arg_list opt_sort_clause ')' Sconst
-- @
data FuncConstArgs = FuncConstArgs (NonEmpty FuncArgExpr) (Maybe SortClause)
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst FuncConstArgs where
  toTextBuilder (FuncConstArgs a b) = commaNonEmpty toTextBuilder a <> suffixMaybe toTextBuilder b
  parser = FuncConstArgs <$> sep1 commaSeparator parser <*> optional (space1 *> parser)

instance Arbitrary FuncConstArgs where
  arbitrary = do
    len <- choose (0, 6)
    x <- scale (`div` 2) arbitrary
    xs <- vectorOf len (scale (`div` 2) arbitrary)
    b <- scale (`div` 2) arbitrary
    pure (FuncConstArgs (x :| xs) b)
