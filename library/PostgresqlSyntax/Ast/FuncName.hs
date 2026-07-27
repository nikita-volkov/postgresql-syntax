module PostgresqlSyntax.Ast.FuncName where

import HeadedMegaparsec
import PostgresqlSyntax.Ast.Ident
import PostgresqlSyntax.Ast.Indirection
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Extras.HeadedMegaparsec hiding (run)
import PostgresqlSyntax.IsAst
import qualified PostgresqlSyntax.KeywordSet as KeywordSet
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import Test.QuickCheck (scale)

-- |
-- 'PostgresqlSyntax.Ast.ColId' and 'PostgresqlSyntax.Ast.TypeFunctionName'
-- are both bare aliases to 'Ident', but their /parsers/ (kept in
-- "PostgresqlSyntax.Parsing" since neither is extracted in this batch) are
-- more permissive than plain 'Ident'. Since this module sits below
-- "PostgresqlSyntax.Parsing" (no import cycle allowed), those flavored
-- element parsers are duplicated here, same as 'PostgresqlSyntax.Ast.NameList'.
--
-- ==== References
-- @
-- func_name:
--   | type_function_name
--   | ColId indirection
-- @
data FuncName
  = TypeFuncName Ident
  | IndirectedFuncName Ident Indirection
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst FuncName where
  toTextBuilder = \case
    TypeFuncName a -> toTextBuilder a
    IndirectedFuncName a b -> toTextBuilder a <> toTextBuilder b
  parser =
    IndirectedFuncName <$> wrapToHead colIdLikeName <*> (space *> parser)
      <|> TypeFuncName <$> typeFunctionNameLikeName
    where
      colIdLikeName =
        label "identifier"
          $ parser
          <|> keywordNameFromSet UnquotedIdent (KeywordSet.unreservedKeyword <> KeywordSet.colNameKeyword)
      typeFunctionNameLikeName =
        keywordNameFromSet UnquotedIdent KeywordSet.typeFunctionName
          <|> parser

instance Arbitrary FuncName where
  arbitrary =
    sized $ \n ->
      if n <= 1
        then TypeFuncName <$> arbitrary
        else
          oneof
            [ TypeFuncName <$> arbitrary,
              IndirectedFuncName <$> arbitrary <*> scale (`div` 2) arbitrary
            ]
