module PostgresqlSyntax.Ast.AexprConst where

import HeadedMegaparsec
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
import PostgresqlSyntax.Extras.HeadedMegaparsec hiding (run)
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import Test.QuickCheck (scale)
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
          space1
          endHead
          a <-
            asum
              [ do
                  a <- parser
                  endHead
                  b <- optional (space1 *> parser)
                  return (StringIntervalAexprConst a b),
                do
                  a <- inParens parser
                  space1
                  endHead
                  b <- parser
                  return (IntIntervalAexprConst a b)
              ]
          return a,
        do
          a <- parser
          space1
          endHead
          b <- parser
          return (ConstTypenameAexprConst a b),
        BoolAexprConst True <$ keyword "true",
        BoolAexprConst False <$ keyword "false",
        NullAexprConst <$ keyword "null" <* parse (Megaparsec.notFollowedBy MegaparsecChar.alphaNumChar),
        either IAexprConst FAexprConst <$> (Right <$> parser <|> Left <$> parser),
        SAexprConst <$> parser,
        BAexprConst <$> parser,
        XAexprConst <$> parser,
        wrapToHead $ do
          a <- parser
          space
          b <- inParens parser
          space1
          d <- parser
          return (FuncAexprConst a (Just b) d),
        FuncAexprConst <$> (wrapToHead parser <* space1) <*> pure Nothing <*> parser
      ]

instance Arbitrary AexprConst where
  arbitrary =
    oneof
      [ IAexprConst <$> arbitrary,
        FAexprConst <$> arbitrary,
        SAexprConst <$> arbitrary,
        BAexprConst <$> arbitrary,
        XAexprConst <$> arbitrary,
        FuncAexprConst <$> arbitrary <*> scale (`div` 2) arbitrary <*> arbitrary,
        ConstTypenameAexprConst <$> arbitrary <*> arbitrary,
        StringIntervalAexprConst <$> arbitrary <*> arbitrary,
        IntIntervalAexprConst <$> arbitrary <*> arbitrary,
        BoolAexprConst <$> arbitrary,
        pure NullAexprConst
      ]
