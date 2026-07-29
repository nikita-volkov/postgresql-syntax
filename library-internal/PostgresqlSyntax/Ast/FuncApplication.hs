module PostgresqlSyntax.Ast.FuncApplication where

import PostgresqlSyntax.Ast.FuncApplicationParams
import PostgresqlSyntax.Ast.FuncName
import qualified PostgresqlSyntax.Helpers.Gens as Gens
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
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
data FuncApplication = FuncApplication FuncName (Maybe FuncApplicationParams)
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst FuncApplication where
  toTextBuilder settings (FuncApplication a b) = toTextBuilder settings a <> "(" <> foldMap (toTextBuilder settings) b <> ")"

  -- \"operator\" immediately followed by \"(\" is always parsed as the start
  -- of a qualified operator (@OPERATOR(...)@), never as a call to a function
  -- literally named \"operator\", mirroring how real PostgreSQL's grammar
  -- resolves the conflict between these two productions in favor of qual_op.
  parser settings =
    Parsers.notFollowedBy (Parsers.keyword "operator" *> Parsers.space *> Parsers.char '(')
      *> Parsers.inParensWithLabel FuncApplication (parser settings) (optional (parser settings))

instance Qc.Arbitrary FuncApplication where
  shrink = Qc.genericShrink
  arbitrary = FuncApplication <$> arbitrary <*> Gens.terminatingMaybe arbitrary
