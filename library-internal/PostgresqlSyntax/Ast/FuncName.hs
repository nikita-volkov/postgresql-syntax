module PostgresqlSyntax.Ast.FuncName where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Ast.Ident
import PostgresqlSyntax.Ast.Indirection
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import PostgresqlSyntax.IsAst
import qualified PostgresqlSyntax.KeywordSet as KeywordSet
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import qualified Test.QuickCheck as Qc

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
  toTextBuilder settings = \case
    TypeFuncName a -> toTextBuilder settings a
    IndirectedFuncName a b -> toTextBuilder settings a <> toTextBuilder settings b
  parser settings =
    IndirectedFuncName
      <$> Parser.wrapToHead colIdLikeName
      <*> (Parsers.space *> parser settings)
        <|> TypeFuncName
      <$> typeFunctionNameLikeName
    where
      colIdLikeName =
        Parser.label "identifier" $
          parser settings
            <|> Parsers.keywordNameFromSet UnquotedIdent (KeywordSet.unreservedKeyword <> KeywordSet.colNameKeyword)
      typeFunctionNameLikeName =
        Parsers.keywordNameFromSet UnquotedIdent KeywordSet.typeFunctionName
          <|> parser settings

instance Qc.Arbitrary FuncName where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.sized $ \n ->
      if n <= 1
        then TypeFuncName <$> Qc.arbitrary
        else
          Qc.oneof
            [ TypeFuncName <$> Qc.arbitrary,
              IndirectedFuncName <$> Qc.arbitrary <*> Qc.arbitrary
            ]
