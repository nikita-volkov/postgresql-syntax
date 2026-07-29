module PostgresqlSyntax.Ast.AexprConst where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Ast.Bconst
import PostgresqlSyntax.Ast.ConstTypename
import PostgresqlSyntax.Ast.Fconst
import PostgresqlSyntax.Ast.FuncConstArgs
import PostgresqlSyntax.Ast.FuncName
import PostgresqlSyntax.Ast.Iconst
import PostgresqlSyntax.Ast.Interval
import PostgresqlSyntax.Ast.Sconst
import PostgresqlSyntax.Ast.Xconst
import qualified PostgresqlSyntax.Helpers.Gens as Gens
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import qualified Test.QuickCheck as Qc
import qualified Text.Megaparsec as Megaparsec
import qualified Text.Megaparsec.Char as MegaparsecChar

-- |
-- ==== References
-- @
-- AexprConst:
--   |  Iconst
--   |  FCONST
--   |  Sconst
--   |  BCONST
--   |  XCONST
--   |  func_name Sconst
--   |  func_name '(' func_arg_list opt_sort_clause ')' Sconst
--   |  ConstTypename Sconst
--   |  ConstInterval Sconst opt_interval
--   |  ConstInterval '(' Iconst ')' Sconst
--   |  TRUE_P
--   |  FALSE_P
--   |  NULL_P
-- @
data AexprConst
  = IAexprConst Iconst
  | FAexprConst Fconst
  | SAexprConst Sconst
  | BAexprConst Bconst
  | XAexprConst Xconst
  | FuncAexprConst FuncName (Maybe FuncConstArgs) Sconst
  | ConstTypenameAexprConst ConstTypename Sconst
  | StringIntervalAexprConst Sconst (Maybe Interval)
  | IntIntervalAexprConst Iconst Sconst
  | BoolAexprConst Bool
  | NullAexprConst
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst AexprConst where
  toTextBuilder settings = \case
    IAexprConst a -> toTextBuilder settings a
    FAexprConst a -> toTextBuilder settings a
    SAexprConst a -> toTextBuilder settings a
    BAexprConst a -> toTextBuilder settings a
    XAexprConst a -> toTextBuilder settings a
    FuncAexprConst a b c -> toTextBuilder settings a <> foldMap (TextBuilders.renderInParens . toTextBuilder settings) b <> " " <> toTextBuilder settings c
    ConstTypenameAexprConst a b -> toTextBuilder settings a <> " " <> toTextBuilder settings b
    StringIntervalAexprConst a b -> "INTERVAL " <> toTextBuilder settings a <> TextBuilders.suffixMaybe (toTextBuilder settings) b
    IntIntervalAexprConst a b -> "INTERVAL " <> TextBuilders.renderInParens (toTextBuilder settings a) <> " " <> toTextBuilder settings b
    BoolAexprConst a -> if a then "TRUE" else "FALSE"
    NullAexprConst -> "NULL"
  parser settings =
    asum
      [ do
          Parsers.keyword "interval"
          Parsers.space1
          Parser.endHead
          a <-
            asum
              [ do
                  a <- parser settings
                  Parser.endHead
                  b <- optional (Parsers.space1 *> parser settings)
                  return (StringIntervalAexprConst a b),
                do
                  a <- Parsers.inParens (parser settings)
                  Parsers.space1
                  Parser.endHead
                  b <- parser settings
                  return (IntIntervalAexprConst a b)
              ]
          return a,
        do
          a <- parser settings
          Parsers.space1
          Parser.endHead
          b <- parser settings
          return (ConstTypenameAexprConst a b),
        BoolAexprConst True <$ Parsers.keyword "true",
        BoolAexprConst False <$ Parsers.keyword "false",
        NullAexprConst <$ Parsers.keyword "null" <* Parser.parse (Megaparsec.notFollowedBy MegaparsecChar.alphaNumChar),
        either IAexprConst FAexprConst <$> (Right <$> parser settings <|> Left <$> parser settings),
        SAexprConst <$> parser settings,
        BAexprConst <$> parser settings,
        XAexprConst <$> parser settings,
        Parser.wrapToHead $ do
          a <- parser settings
          Parsers.space
          b <- Parsers.inParens (parser settings)
          Parsers.space1
          d <- parser settings
          return (FuncAexprConst a (Just b) d),
        FuncAexprConst <$> (Parser.wrapToHead (parser settings) <* Parsers.space1) <*> pure Nothing <*> parser settings
      ]

instance Qc.Arbitrary AexprConst where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.oneof
      [ IAexprConst <$> Qc.arbitrary,
        FAexprConst <$> Qc.arbitrary,
        SAexprConst <$> Qc.arbitrary,
        BAexprConst <$> Qc.arbitrary,
        XAexprConst <$> Qc.arbitrary,
        FuncAexprConst <$> Qc.arbitrary <*> Gens.terminatingMaybe Qc.arbitrary <*> Qc.arbitrary,
        ConstTypenameAexprConst <$> Qc.arbitrary <*> Qc.arbitrary,
        StringIntervalAexprConst <$> Qc.arbitrary <*> Qc.arbitrary,
        IntIntervalAexprConst <$> Qc.arbitrary <*> Qc.arbitrary,
        BoolAexprConst <$> Qc.arbitrary,
        pure NullAexprConst
      ]
