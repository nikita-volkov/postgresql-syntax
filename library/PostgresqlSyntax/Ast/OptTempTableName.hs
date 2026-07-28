module PostgresqlSyntax.Ast.OptTempTableName where

import Control.Applicative.Combinators (option)
import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Ast.QualifiedName
import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Parser
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
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
              [ TemporaryOptTempTableName <$ keyword "temporary" <* Parser.space1,
                TempOptTempTableName <$ keyword "temp" <* Parser.space1,
                LocalTemporaryOptTempTableName <$ keyphrase "local temporary" <* Parser.space1,
                LocalTempOptTempTableName <$ keyphrase "local temp" <* Parser.space1,
                GlobalTemporaryOptTempTableName <$ keyphrase "global temporary" <* Parser.space1,
                GlobalTempOptTempTableName <$ keyphrase "global temp" <* Parser.space1,
                UnloggedOptTempTableName <$ keyword "unlogged" <* Parser.space1
              ]
          b <- option False (True <$ keyword "table" <* Parser.space1)
          c <- parser
          return (a b c),
        do
          keyword "table"
          Parser.space1
          Parser.endHead
          TableOptTempTableName <$> parser,
        QualifedOptTempTableName <$> parser
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
