module PostgresqlSyntax.Ast.AexprConst where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Ast.Bconst
import PostgresqlSyntax.Ast.ConstTypename
import PostgresqlSyntax.Ast.Fconst
import PostgresqlSyntax.Ast.FuncConstArgs
import PostgresqlSyntax.Ast.FuncName
import PostgresqlSyntax.Ast.Iconst
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Ast.Interval
import PostgresqlSyntax.Ast.Sconst
import PostgresqlSyntax.Ast.Xconst
import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Parser
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
  toTextBuilder = \case
    IAexprConst a -> toTextBuilder a
    FAexprConst a -> toTextBuilder a
    SAexprConst a -> toTextBuilder a
    BAexprConst a -> toTextBuilder a
    XAexprConst a -> toTextBuilder a
    FuncAexprConst a b c -> toTextBuilder a <> foldMap (renderInParens . toTextBuilder) b <> " " <> toTextBuilder c
    ConstTypenameAexprConst a b -> toTextBuilder a <> " " <> toTextBuilder b
    StringIntervalAexprConst a b -> "INTERVAL " <> toTextBuilder a <> suffixMaybe toTextBuilder b
    IntIntervalAexprConst a b -> "INTERVAL " <> renderInParens (toTextBuilder a) <> " " <> toTextBuilder b
    BoolAexprConst a -> if a then "TRUE" else "FALSE"
    NullAexprConst -> "NULL"
  parser =
    asum
      [ do
          keyword "interval"
          Parser.space1
          Parser.endHead
          a <-
            asum
              [ do
                  a <- parser
                  Parser.endHead
                  b <- optional (Parser.space1 *> parser)
                  return (StringIntervalAexprConst a b),
                do
                  a <- inParens parser
                  Parser.space1
                  Parser.endHead
                  b <- parser
                  return (IntIntervalAexprConst a b)
              ]
          return a,
        do
          a <- parser
          Parser.space1
          Parser.endHead
          b <- parser
          return (ConstTypenameAexprConst a b),
        BoolAexprConst True <$ keyword "true",
        BoolAexprConst False <$ keyword "false",
        NullAexprConst <$ keyword "null" <* Parser.parse (Megaparsec.notFollowedBy MegaparsecChar.alphaNumChar),
        either IAexprConst FAexprConst <$> (Right <$> parser <|> Left <$> parser),
        SAexprConst <$> parser,
        BAexprConst <$> parser,
        XAexprConst <$> parser,
        Parser.wrapToHead $ do
          a <- parser
          Parser.space
          b <- inParens parser
          Parser.space1
          d <- parser
          return (FuncAexprConst a (Just b) d),
        FuncAexprConst <$> (Parser.wrapToHead parser <* Parser.space1) <*> pure Nothing <*> parser
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
        FuncAexprConst <$> Qc.arbitrary <*> Qc.scale (`div` 2) Qc.arbitrary <*> Qc.arbitrary,
        ConstTypenameAexprConst <$> Qc.arbitrary <*> Qc.arbitrary,
        StringIntervalAexprConst <$> Qc.arbitrary <*> Qc.arbitrary,
        IntIntervalAexprConst <$> Qc.arbitrary <*> Qc.arbitrary,
        BoolAexprConst <$> Qc.arbitrary,
        pure NullAexprConst
      ]
