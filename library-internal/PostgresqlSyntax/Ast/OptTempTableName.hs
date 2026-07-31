module PostgresqlSyntax.Ast.OptTempTableName where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Algebra
import PostgresqlSyntax.Ast.QualifiedName
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
import PostgresqlSyntax.Prelude
import qualified Test.QuickCheck as Qc

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
  toTextBuilder settings = \case
    TemporaryOptTempTableName a b -> TextBuilders.optLexemes [Just "TEMPORARY", bool Nothing (Just "TABLE") a, Just (toTextBuilder settings b)]
    TempOptTempTableName a b -> TextBuilders.optLexemes [Just "TEMP", bool Nothing (Just "TABLE") a, Just (toTextBuilder settings b)]
    LocalTemporaryOptTempTableName a b -> TextBuilders.optLexemes [Just "LOCAL TEMPORARY", bool Nothing (Just "TABLE") a, Just (toTextBuilder settings b)]
    LocalTempOptTempTableName a b -> TextBuilders.optLexemes [Just "LOCAL TEMP", bool Nothing (Just "TABLE") a, Just (toTextBuilder settings b)]
    GlobalTemporaryOptTempTableName a b -> TextBuilders.optLexemes [Just "GLOBAL TEMPORARY", bool Nothing (Just "TABLE") a, Just (toTextBuilder settings b)]
    GlobalTempOptTempTableName a b -> TextBuilders.optLexemes [Just "GLOBAL TEMP", bool Nothing (Just "TABLE") a, Just (toTextBuilder settings b)]
    UnloggedOptTempTableName a b -> TextBuilders.optLexemes [Just "UNLOGGED", bool Nothing (Just "TABLE") a, Just (toTextBuilder settings b)]
    TableOptTempTableName a -> "TABLE " <> toTextBuilder settings a
    QualifedOptTempTableName a -> toTextBuilder settings a
  parser settings =
    asum
      [ do
          a <-
            asum
              [ TemporaryOptTempTableName <$ Parsers.keyword "temporary" <* Parsers.space1,
                TempOptTempTableName <$ Parsers.keyword "temp" <* Parsers.space1,
                LocalTemporaryOptTempTableName <$ Parsers.keyphrase "local temporary" <* Parsers.space1,
                LocalTempOptTempTableName <$ Parsers.keyphrase "local temp" <* Parsers.space1,
                GlobalTemporaryOptTempTableName <$ Parsers.keyphrase "global temporary" <* Parsers.space1,
                GlobalTempOptTempTableName <$ Parsers.keyphrase "global temp" <* Parsers.space1,
                UnloggedOptTempTableName <$ Parsers.keyword "unlogged" <* Parsers.space1
              ]
          b <- option False (True <$ Parsers.keyword "table" <* Parsers.space1)
          c <- parser settings
          return (a b c),
        do
          Parsers.keyword "table"
          Parsers.space1
          Parser.endHead
          TableOptTempTableName <$> parser settings,
        QualifedOptTempTableName <$> parser settings
      ]

instance Qc.Arbitrary OptTempTableName where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.oneof
      [ TemporaryOptTempTableName <$> Qc.arbitrary <*> Qc.arbitrary,
        TempOptTempTableName <$> Qc.arbitrary <*> Qc.arbitrary,
        LocalTemporaryOptTempTableName <$> Qc.arbitrary <*> Qc.arbitrary,
        LocalTempOptTempTableName <$> Qc.arbitrary <*> Qc.arbitrary,
        GlobalTemporaryOptTempTableName <$> Qc.arbitrary <*> Qc.arbitrary,
        GlobalTempOptTempTableName <$> Qc.arbitrary <*> Qc.arbitrary,
        UnloggedOptTempTableName <$> Qc.arbitrary <*> Qc.arbitrary,
        TableOptTempTableName <$> Qc.arbitrary,
        QualifedOptTempTableName <$> Qc.arbitrary
      ]
