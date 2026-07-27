module PostgresqlSyntax.Ast.AExpr
  ( AExpr (..),
    filteredParser,
  )
where

import HeadedMegaparsec
import PostgresqlSyntax.Ast.AExprReversableOp
import qualified PostgresqlSyntax.Ast.CExpr as CExpr
import PostgresqlSyntax.Ast.CExpr (CExpr)
import PostgresqlSyntax.Ast.Ident
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Ast.QualOp
import PostgresqlSyntax.Ast.Row
import PostgresqlSyntax.Ast.SubqueryOp
import PostgresqlSyntax.Ast.SubType
import PostgresqlSyntax.Ast.SymbolicExprBinOp
import PostgresqlSyntax.Ast.Typename
import PostgresqlSyntax.Ast.VerbalExprBinOp
import PostgresqlSyntax.Ast.AnyName hiding (filteredParser)
import {-# SOURCE #-} PostgresqlSyntax.Ast.SelectWithParens (SelectWithParens)
import PostgresqlSyntax.Extras.HeadedMegaparsec hiding (run)
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import Test.QuickCheck (scale)

-- |
-- ==== References
-- @
-- a_expr:
--   | c_expr
--   | a_expr TYPECAST Typename
--   | a_expr COLLATE any_name
--   | a_expr AT TIME ZONE a_expr
--   | '+' a_expr
--   | '-' a_expr
--   | a_expr '+' a_expr
--   | a_expr '-' a_expr
--   | a_expr '*' a_expr
--   | a_expr '/' a_expr
--   | a_expr '%' a_expr
--   | a_expr '^' a_expr
--   | a_expr '<' a_expr
--   | a_expr '>' a_expr
--   | a_expr '=' a_expr
--   | a_expr LESS_EQUALS a_expr
--   | a_expr GREATER_EQUALS a_expr
--   | a_expr NOT_EQUALS a_expr
--   | a_expr qual_Op a_expr
--   | qual_Op a_expr
--   | a_expr qual_Op
--   | a_expr AND a_expr
--   | a_expr OR a_expr
--   | NOT a_expr
--   | NOT_LA a_expr
--   | a_expr LIKE a_expr
--   | a_expr LIKE a_expr ESCAPE a_expr
--   | a_expr NOT_LA LIKE a_expr
--   | a_expr NOT_LA LIKE a_expr ESCAPE a_expr
--   | a_expr ILIKE a_expr
--   | a_expr ILIKE a_expr ESCAPE a_expr
--   | a_expr NOT_LA ILIKE a_expr
--   | a_expr NOT_LA ILIKE a_expr ESCAPE a_expr
--   | a_expr SIMILAR TO a_expr
--   | a_expr SIMILAR TO a_expr ESCAPE a_expr
--   | a_expr NOT_LA SIMILAR TO a_expr
--   | a_expr NOT_LA SIMILAR TO a_expr ESCAPE a_expr
--   | a_expr IS NULL_P
--   | a_expr ISNULL
--   | a_expr IS NOT NULL_P
--   | a_expr NOTNULL
--   | row OVERLAPS row
--   | a_expr IS TRUE_P
--   | a_expr IS NOT TRUE_P
--   | a_expr IS FALSE_P
--   | a_expr IS NOT FALSE_P
--   | a_expr IS UNKNOWN
--   | a_expr IS NOT UNKNOWN
--   | a_expr IS DISTINCT FROM a_expr
--   | a_expr IS NOT DISTINCT FROM a_expr
--   | a_expr IS OF '(' type_list ')'
--   | a_expr IS NOT OF '(' type_list ')'
--   | a_expr BETWEEN opt_asymmetric b_expr AND a_expr
--   | a_expr NOT_LA BETWEEN opt_asymmetric b_expr AND a_expr
--   | a_expr BETWEEN SYMMETRIC b_expr AND a_expr
--   | a_expr NOT_LA BETWEEN SYMMETRIC b_expr AND a_expr
--   | a_expr IN_P in_expr
--   | a_expr NOT_LA IN_P in_expr
--   | a_expr subquery_Op sub_type select_with_parens
--   | a_expr subquery_Op sub_type '(' a_expr ')'
--   | UNIQUE select_with_parens
--   | a_expr IS DOCUMENT_P
--   | a_expr IS NOT DOCUMENT_P
--   | DEFAULT
-- @
data AExpr
  = CExprAExpr CExpr
  | TypecastAExpr AExpr Typename
  | CollateAExpr AExpr AnyName
  | AtTimeZoneAExpr AExpr AExpr
  | PlusAExpr AExpr
  | MinusAExpr AExpr
  | SymbolicBinOpAExpr AExpr SymbolicExprBinOp AExpr
  | PrefixQualOpAExpr QualOp AExpr
  | SuffixQualOpAExpr AExpr QualOp
  | AndAExpr AExpr AExpr
  | OrAExpr AExpr AExpr
  | NotAExpr AExpr
  | VerbalExprBinOpAExpr AExpr Bool VerbalExprBinOp AExpr (Maybe AExpr)
  | ReversableOpAExpr AExpr Bool AExprReversableOp
  | IsnullAExpr AExpr
  | NotnullAExpr AExpr
  | OverlapsAExpr Row Row
  | SubqueryAExpr AExpr SubqueryOp SubType (Either SelectWithParens AExpr)
  | UniqueAExpr SelectWithParens
  | DefaultAExpr
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst AExpr where
  toTextBuilder = \case
    CExprAExpr a -> toTextBuilder a
    TypecastAExpr a b -> toTextBuilder a <> " :: " <> toTextBuilder b
    CollateAExpr a b -> toTextBuilder a <> " COLLATE " <> toTextBuilder b
    AtTimeZoneAExpr a b -> toTextBuilder a <> " AT TIME ZONE " <> toTextBuilder b
    PlusAExpr a -> "+ " <> toTextBuilder a
    MinusAExpr a -> "- " <> toTextBuilder a
    SymbolicBinOpAExpr a b c -> toTextBuilder a <> " " <> toTextBuilder b <> " " <> toTextBuilder c
    PrefixQualOpAExpr a b -> toTextBuilder a <> " " <> toTextBuilder b
    SuffixQualOpAExpr a b -> toTextBuilder a <> " " <> toTextBuilder b
    AndAExpr a b -> toTextBuilder a <> " AND " <> toTextBuilder b
    OrAExpr a b -> toTextBuilder a <> " OR " <> toTextBuilder b
    NotAExpr a -> "NOT " <> toTextBuilder a
    VerbalExprBinOpAExpr a b c d e -> toTextBuilder a <> " " <> bool "" "NOT " b <> toTextBuilder c <> " " <> toTextBuilder d <> foldMap (mappend " ESCAPE " . toTextBuilder) e
    ReversableOpAExpr a b c -> toTextBuilder a <> " " <> renderAExprReversableOp b c
    IsnullAExpr a -> toTextBuilder a <> " ISNULL"
    NotnullAExpr a -> toTextBuilder a <> " NOTNULL"
    OverlapsAExpr a b -> toTextBuilder a <> " OVERLAPS " <> toTextBuilder b
    SubqueryAExpr a b c d -> toTextBuilder a <> " " <> toTextBuilder b <> " " <> toTextBuilder c <> " " <> either toTextBuilder (renderInParens . toTextBuilder) d
    UniqueAExpr a -> "UNIQUE " <> toTextBuilder a
    DefaultAExpr -> "DEFAULT"
    where
      -- |
      -- Distinct from 'PostgresqlSyntax.Ast.AExprReversableOp'\'s own
      -- @toTextBuilder@ (which bakes in the "positive" @IS@\/@BETWEEN@\/@IN@
      -- keyword but not the negation) — this one threads the external
      -- @Bool@ (@NOT@) in, mirroring the pre-extraction top-level
      -- @aExprReversableOp@ renderer.
      renderAExprReversableOp a = \case
        NullAExprReversableOp -> bool "IS " "IS NOT " a <> "NULL"
        TrueAExprReversableOp -> bool "IS " "IS NOT " a <> "TRUE"
        FalseAExprReversableOp -> bool "IS " "IS NOT " a <> "FALSE"
        UnknownAExprReversableOp -> bool "IS " "IS NOT " a <> "UNKNOWN"
        DistinctFromAExprReversableOp b -> bool "IS " "IS NOT " a <> "DISTINCT FROM " <> toTextBuilder b
        OfAExprReversableOp b -> bool "IS " "IS NOT " a <> "OF " <> renderInParens (toTextBuilder b)
        BetweenAExprReversableOp b c d -> bool "" "NOT " a <> bool "BETWEEN " "BETWEEN ASYMMETRIC " b <> toTextBuilder c <> " AND " <> toTextBuilder d
        BetweenSymmetricAExprReversableOp b c -> bool "" "NOT " a <> "BETWEEN SYMMETRIC " <> toTextBuilder b <> " AND " <> toTextBuilder c
        InAExprReversableOp b -> bool "" "NOT " a <> "IN " <> toTextBuilder b
        DocumentAExprReversableOp -> bool "IS " "IS NOT " a <> "DOCUMENT"
  parser = customizedParser parser

-- |
-- Parameterized over the 'PostgresqlSyntax.Ast.CExpr' parser embedded at the
-- base case — the only axis 'filteredParser' needs to customize (via
-- 'PostgresqlSyntax.Ast.CExpr.customizedParser'). Every other occurrence of
-- @a_expr@\/@b_expr@\/@select_with_parens@ in the grammar below uses the
-- ordinary, unfiltered parsers, exactly as the pre-extraction
-- @customizedAExpr@ did (its @bExpr@\/@selectWithParens@ references were
-- never threaded through the @cExpr@ parameter either).
customizedParser :: Parser CExpr -> Parser AExpr
customizedParser cExpr = suffixRec base suffix
  where
    aExpr = customizedParser cExpr
    base =
      asum
        [ DefaultAExpr <$ keyword "default",
          UniqueAExpr <$> (keyword "unique" *> space1 *> parser),
          qualOpExpr aExpr PrefixQualOpAExpr,
          PlusAExpr <$> plusedExpr aExpr,
          MinusAExpr <$> minusedExpr aExpr,
          NotAExpr <$> (keyword "not" *> space1 *> aExpr),
          CExprAExpr <$> cExpr
        ]
    suffix a =
      asum
        [ overlapsSuffix a,
          do
            space1
            b <- wrapToHead parser
            space1
            c <- wrapToHead parser
            space
            d <- Left <$> wrapToHead parser <|> Right <$> inParens aExpr
            return (SubqueryAExpr a b c d),
          typecastExpr a TypecastAExpr,
          CollateAExpr a <$> (space1 *> keyword "collate" *> space1 *> endHead *> parser),
          AtTimeZoneAExpr a <$> (space1 *> keyphrase "at time zone" *> space1 *> endHead *> aExpr),
          symbolicBinOpExpr a aExpr SymbolicBinOpAExpr,
          SuffixQualOpAExpr a <$> (space *> parser),
          AndAExpr a <$> (space1 *> keyword "and" *> space1 *> endHead *> aExpr),
          OrAExpr a <$> (space1 *> keyword "or" *> space1 *> endHead *> aExpr),
          do
            space1
            b <- trueIfPresent (keyword "not" *> space1)
            c <- parser
            space1
            endHead
            d <- aExpr
            e <- optional (space1 *> keyword "escape" *> space1 *> endHead *> aExpr)
            return (VerbalExprBinOpAExpr a b c d e),
          do
            space1
            keyword "is"
            space1
            endHead
            b <- trueIfPresent (keyword "not" *> space1)
            c <-
              asum
                [ NullAExprReversableOp <$ keyword "null",
                  TrueAExprReversableOp <$ keyword "true",
                  FalseAExprReversableOp <$ keyword "false",
                  UnknownAExprReversableOp <$ keyword "unknown",
                  DistinctFromAExprReversableOp <$> (keyword "distinct" *> space1 *> keyword "from" *> space1 *> endHead *> aExpr),
                  OfAExprReversableOp <$> (keyword "of" *> space1 *> endHead *> inParens parser),
                  DocumentAExprReversableOp <$ keyword "document"
                ]
            return (ReversableOpAExpr a b c),
          do
            space1
            b <- trueIfPresent (keyword "not" *> space1)
            keyword "between"
            space1
            endHead
            c <-
              asum
                [ BetweenSymmetricAExprReversableOp <$ (keyword "symmetric" *> space1),
                  BetweenAExprReversableOp True <$ (keyword "asymmetric" *> space1),
                  pure (BetweenAExprReversableOp False)
                ]
            d <- parser
            space1
            keyword "and"
            space1
            e <- aExpr
            return (ReversableOpAExpr a b (c d e)),
          do
            space1
            b <- trueIfPresent (keyword "not" *> space1)
            keyword "in"
            space
            c <- InAExprReversableOp <$> parser
            return (ReversableOpAExpr a b c),
          IsnullAExpr a <$ (space1 *> keyword "isnull"),
          NotnullAExpr a <$ (space1 *> keyword "notnull")
        ]

-- |
-- The @OVERLAPS@ operator, as a suffix of an already parsed left operand.
-- Reinterprets the already-parsed base 'AExpr' as a 'Row' rather than
-- speculatively parsing a @row@ on top of it — see the original
-- @overlapsSuffix@ in the pre-extraction @PostgresqlSyntax.Parsing@ for the
-- exponential-blowup rationale this avoids.
overlapsSuffix :: AExpr -> Parser AExpr
overlapsSuffix a = do
  b <- maybe empty pure (aExprRow a)
  space1
  keyword "overlaps"
  endHead
  space1
  c <- parser
  return (OverlapsAExpr b c)
  where
    aExprRow = \case
      CExprAExpr (CExpr.ExplicitRowCExpr x) -> Just (ExplicitRowRow x)
      CExprAExpr (CExpr.ImplicitRowCExpr x) -> Just (ImplicitRowRow x)
      _ -> Nothing

-- | 'parser', but rejecting the given words when they'd otherwise be
-- accepted as a trailing bare column-reference identifier. Needed by
-- "PostgresqlSyntax.Ast.SortBy", which must not let @a_expr@ swallow a
-- keyword (@USING@\/@ASC@\/@DESC@\/@NULLS@) that is meant to terminate it.
filteredParser :: [Text] -> Parser AExpr
filteredParser excluded = customizedParser (CExpr.customizedParser (filteredColIdLike UnquotedIdent parser excluded))

instance Arbitrary AExpr where
  arbitrary =
    sized $ \n ->
      if n <= 1
        then oneof [CExprAExpr <$> scale (`div` 2) arbitrary, pure DefaultAExpr]
        else
          oneof
            [ CExprAExpr <$> scale (`div` 2) arbitrary,
              pure DefaultAExpr,
              TypecastAExpr <$> scale (`div` 2) arbitrary <*> arbitrary,
              CollateAExpr <$> scale (`div` 2) arbitrary <*> arbitrary,
              AtTimeZoneAExpr <$> scale (`div` 2) arbitrary <*> scale (`div` 2) arbitrary,
              PlusAExpr <$> scale (`div` 2) arbitrary,
              MinusAExpr <$> scale (`div` 2) arbitrary,
              SymbolicBinOpAExpr <$> scale (`div` 2) arbitrary <*> arbitrary <*> scale (`div` 2) arbitrary,
              PrefixQualOpAExpr <$> arbitrary <*> scale (`div` 2) arbitrary,
              SuffixQualOpAExpr <$> scale (`div` 2) arbitrary <*> arbitrary,
              AndAExpr <$> scale (`div` 2) arbitrary <*> scale (`div` 2) arbitrary,
              OrAExpr <$> scale (`div` 2) arbitrary <*> scale (`div` 2) arbitrary,
              NotAExpr <$> scale (`div` 2) arbitrary,
              VerbalExprBinOpAExpr <$> scale (`div` 4) arbitrary <*> arbitrary <*> arbitrary <*> scale (`div` 4) arbitrary <*> scale (`div` 4) arbitrary,
              ReversableOpAExpr <$> scale (`div` 2) arbitrary <*> arbitrary <*> scale (`div` 2) arbitrary,
              IsnullAExpr <$> scale (`div` 2) arbitrary,
              NotnullAExpr <$> scale (`div` 2) arbitrary,
              OverlapsAExpr <$> scale (`div` 2) arbitrary <*> scale (`div` 2) arbitrary,
              SubqueryAExpr <$> scale (`div` 4) arbitrary <*> arbitrary <*> arbitrary <*> scale (`div` 4) arbitrary,
              UniqueAExpr <$> scale (`div` 2) arbitrary
            ]
