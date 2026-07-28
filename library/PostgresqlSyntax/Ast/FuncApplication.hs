module PostgresqlSyntax.Ast.FuncApplication where

import PostgresqlSyntax.Ast.FuncApplicationParams
import PostgresqlSyntax.Ast.FuncName
import PostgresqlSyntax.Ast.Internal
import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Parser
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
  toTextBuilder (FuncApplication a b) = toTextBuilder a <> "(" <> foldMap toTextBuilder b <> ")"

  -- \"operator\" immediately followed by \"(\" is always parsed as the start
  -- of a qualified operator (@OPERATOR(...)@), never as a call to a function
  -- literally named \"operator\", mirroring how real PostgreSQL's grammar
  -- resolves the conflict between these two productions in favor of qual_op.
  parser =
    Parser.notFollowedBy (keyword "operator" *> Parser.space *> Parser.char '(')
      *> inParensWithLabel FuncApplication parser (optional parser)

instance Qc.Arbitrary FuncApplication where
  arbitrary = FuncApplication <$> arbitrary <*> Qc.scale (`div` 2) arbitrary
