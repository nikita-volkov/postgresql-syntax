module PostgresqlSyntax.Ast.OptTempTableName where

import Control.Applicative.Combinators (option)
import HeadedMegaparsec
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Ast.QualifiedName
import PostgresqlSyntax.Extras.HeadedMegaparsec hiding (run)
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)

-- |
-- ==== References
-- @
-- OptTempTableName:
--   |  TEMPORARY opt_table qualified_name
--   |  TEMP opt_table qualified_name
--   |  LOCAL TEMPORARY opt_table qualified_name
--   |  LOCAL TEMP opt_table qualified_name
--   |  GLOBAL TEMPORARY opt_table qualified_name
--   |  GLOBAL TEMP opt_table qualified_name
--   |  UNLOGGED opt_table qualified_name
--   |  TABLE qualified_name
--   |  qualified_name
-- @
data OptTempTableName
  = TemporaryOptTempTableName Bool QualifiedName
  | TempOptTempTableName Bool QualifiedName
  | LocalTemporaryOptTempTableName Bool QualifiedName
  | LocalTempOptTempTableName Bool QualifiedName
  | GlobalTemporaryOptTempTableName Bool QualifiedName
  | GlobalTempOptTempTableName Bool QualifiedName
  | UnloggedOptTempTableName Bool QualifiedName
  | TableOptTempTableName QualifiedName
  | QualifedOptTempTableName QualifiedName
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst OptTempTableName where
  toTextBuilder = \case
    TemporaryOptTempTableName a b -> optLexemes [Just "TEMPORARY", bool Nothing (Just "TABLE") a, Just (toTextBuilder b)]
    TempOptTempTableName a b -> optLexemes [Just "TEMP", bool Nothing (Just "TABLE") a, Just (toTextBuilder b)]
    LocalTemporaryOptTempTableName a b -> optLexemes [Just "LOCAL TEMPORARY", bool Nothing (Just "TABLE") a, Just (toTextBuilder b)]
    LocalTempOptTempTableName a b -> optLexemes [Just "LOCAL TEMP", bool Nothing (Just "TABLE") a, Just (toTextBuilder b)]
    GlobalTemporaryOptTempTableName a b -> optLexemes [Just "GLOBAL TEMPORARY", bool Nothing (Just "TABLE") a, Just (toTextBuilder b)]
    GlobalTempOptTempTableName a b -> optLexemes [Just "GLOBAL TEMP", bool Nothing (Just "TABLE") a, Just (toTextBuilder b)]
    UnloggedOptTempTableName a b -> optLexemes [Just "UNLOGGED", bool Nothing (Just "TABLE") a, Just (toTextBuilder b)]
    TableOptTempTableName a -> "TABLE " <> toTextBuilder a
    QualifedOptTempTableName a -> toTextBuilder a
  parser =
    asum
      [ do
          a <-
            asum
              [ TemporaryOptTempTableName <$ keyword "temporary" <* space1,
                TempOptTempTableName <$ keyword "temp" <* space1,
                LocalTemporaryOptTempTableName <$ keyphrase "local temporary" <* space1,
                LocalTempOptTempTableName <$ keyphrase "local temp" <* space1,
                GlobalTemporaryOptTempTableName <$ keyphrase "global temporary" <* space1,
                GlobalTempOptTempTableName <$ keyphrase "global temp" <* space1,
                UnloggedOptTempTableName <$ keyword "unlogged" <* space1
              ]
          b <- option False (True <$ keyword "table" <* space1)
          c <- parser
          return (a b c),
        do
          keyword "table"
          space1
          endHead
          TableOptTempTableName <$> parser,
        QualifedOptTempTableName <$> parser
      ]

instance Arbitrary OptTempTableName where
  arbitrary =
    oneof
      [ TemporaryOptTempTableName <$> arbitrary <*> arbitrary,
        TempOptTempTableName <$> arbitrary <*> arbitrary,
        LocalTemporaryOptTempTableName <$> arbitrary <*> arbitrary,
        LocalTempOptTempTableName <$> arbitrary <*> arbitrary,
        GlobalTemporaryOptTempTableName <$> arbitrary <*> arbitrary,
        GlobalTempOptTempTableName <$> arbitrary <*> arbitrary,
        UnloggedOptTempTableName <$> arbitrary <*> arbitrary,
        TableOptTempTableName <$> arbitrary,
        QualifedOptTempTableName <$> arbitrary
      ]
