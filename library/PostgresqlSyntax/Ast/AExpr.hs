module PostgresqlSyntax.Ast.AExpr
  ( AExpr (..),
    filteredParser,
    isBoundedAExprOperand,
    safeAExprOperand,
  )
where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Ast.AExprReversableOp
import PostgresqlSyntax.Ast.AnyName hiding (filteredParser)
import PostgresqlSyntax.Ast.CExpr (CExpr)
import qualified PostgresqlSyntax.Ast.CExpr as CExpr
import PostgresqlSyntax.Ast.Ident
import PostgresqlSyntax.Ast.QualOp
import PostgresqlSyntax.Ast.Row
import {-# SOURCE #-} PostgresqlSyntax.Ast.SelectWithParens (SelectWithParens)
import PostgresqlSyntax.Ast.SubType
import PostgresqlSyntax.Ast.SubqueryOp
import PostgresqlSyntax.Ast.SymbolicExprBinOp
import PostgresqlSyntax.Ast.Typename
import PostgresqlSyntax.Ast.VerbalExprBinOp
import qualified PostgresqlSyntax.Extras.QuickCheck as Qc
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import qualified Test.QuickCheck as Qc

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
    TypecastAExpr a b -> renderOperand a <> " :: " <> toTextBuilder b
    CollateAExpr a b -> renderOperand a <> " COLLATE " <> toTextBuilder b
    AtTimeZoneAExpr a b -> renderOperand a <> " AT TIME ZONE " <> toTextBuilder b
    PlusAExpr a -> "+ " <> toTextBuilder a
    MinusAExpr a -> "- " <> toTextBuilder a
    SymbolicBinOpAExpr a b c -> renderOperand a <> " " <> toTextBuilder b <> " " <> toTextBuilder c
    PrefixQualOpAExpr a b -> toTextBuilder a <> " " <> toTextBuilder b
    SuffixQualOpAExpr a b -> renderOperand a <> " " <> toTextBuilder b
    AndAExpr a b -> renderOperand a <> " AND " <> toTextBuilder b
    OrAExpr a b -> renderOperand a <> " OR " <> toTextBuilder b
    NotAExpr a -> "NOT " <> toTextBuilder a
    VerbalExprBinOpAExpr a b c d e -> renderOperand a <> " " <> bool "" "NOT " b <> toTextBuilder c <> " " <> renderVerbalRhs e d <> foldMap (mappend " ESCAPE " . toTextBuilder) e
    ReversableOpAExpr a b c -> renderOperand a <> " " <> renderAExprReversableOp b c
    IsnullAExpr a -> renderOperand a <> " ISNULL"
    NotnullAExpr a -> renderOperand a <> " NOTNULL"
    OverlapsAExpr a b -> toTextBuilder a <> " OVERLAPS " <> toTextBuilder b
    SubqueryAExpr a b c d -> renderOperand a <> " " <> toTextBuilder b <> " " <> toTextBuilder c <> " " <> either toTextBuilder (TextBuilders.renderInParens . toTextBuilder) d
    UniqueAExpr a -> "UNIQUE " <> toTextBuilder a
    DefaultAExpr -> "DEFAULT"
    where
      -- Renders an operand sitting in the left\/accumulator position of a
      -- suffix production (the @a@ that 'customizedParser'\'s @suffixRec@
      -- threads through). Every alternative in @suffix@ parses its
      -- right-hand operand via an unrestricted recursive @a_expr@ call, so
      -- rendering it plainly always reconstructs the same shape on reparse.
      -- The *left* position is different: control only returns to
      -- @suffixRec@'s loop after a __bounded__ production — one that
      -- doesn't itself end in an unrestricted recursive @a_expr@. A value
      -- shaped like 'PlusAExpr'\/'MinusAExpr'\/'NotAExpr'\/
      -- 'PrefixQualOpAExpr'\/'AtTimeZoneAExpr'\/'SymbolicBinOpAExpr'\/
      -- 'AndAExpr'\/'OrAExpr'\/'VerbalExprBinOpAExpr', or a 'ReversableOpAExpr'
      -- whose 'PostgresqlSyntax.Ast.AExprReversableOp' ends in one too, is
      -- unbounded — placed there bare, it would greedily re-absorb whatever
      -- follows on reparse (e.g. rendering @SymbolicBinOpAExpr (NotAExpr x)
      -- op y@ plainly as @NOT x op y@ reparses as @NotAExpr (SymbolicBinOpAExpr
      -- x op y)@). Parenthesizing it, via the existing @'(' a_expr ')'@
      -- 'PostgresqlSyntax.Ast.CExpr' production, keeps rendering correct for
      -- any 'AExpr' value, whether or not the parser itself could have
      -- produced it in that position.
      renderOperand a
        | isBoundedAExprOperand a = toTextBuilder a
        | otherwise = TextBuilders.renderInParens (toTextBuilder a)
      -- The @d@ operand of a @LIKE@\/@ILIKE@\/@SIMILAR TO@ production, when
      -- it has a trailing @ESCAPE@ clause of its own (@e@). Since @d@ is
      -- parsed via an unrestricted recursive @a_expr@, rendered bare it
      -- could itself end in a dangling operator or an escape-less
      -- 'VerbalExprBinOpAExpr' — either of which would, on reparse, greedily
      -- swallow the text meant for *this* production's own @ESCAPE@ (an
      -- operand identifier in the first case, its own inner escape clause in
      -- the second). Parenthesizing @d@ whenever @e@ is present rules that
      -- out, the same way @renderOperand@ does for the left position.
      renderVerbalRhs e d = case e of
        Nothing -> toTextBuilder d
        -- Already explicitly parenthesized (as the 'Qc.Arbitrary'
        -- instance below always arranges whenever it generates an escape
        -- clause) — render as-is instead of adding a second, redundant
        -- layer of parens.
        Just _
          | CExprAExpr (CExpr.InParensCExpr _ _) <- d -> toTextBuilder d
          | otherwise -> TextBuilders.renderInParens (toTextBuilder d)
      -- Distinct from 'PostgresqlSyntax.Ast.AExprReversableOp'\'s own
      -- @toTextBuilder@ (which bakes in the "positive" @IS@\/@BETWEEN@\/@IN@
      -- Parsers.keyword but not the negation) — this one threads the external
      -- @Bool@ (@NOT@) in, mirroring the pre-extraction top-level
      -- @aExprReversableOp@ renderer.
      renderAExprReversableOp a = \case
        NullAExprReversableOp -> bool "IS " "IS NOT " a <> "NULL"
        TrueAExprReversableOp -> bool "IS " "IS NOT " a <> "TRUE"
        FalseAExprReversableOp -> bool "IS " "IS NOT " a <> "FALSE"
        UnknownAExprReversableOp -> bool "IS " "IS NOT " a <> "UNKNOWN"
        DistinctFromAExprReversableOp b -> bool "IS " "IS NOT " a <> "DISTINCT FROM " <> toTextBuilder b
        OfAExprReversableOp b -> bool "IS " "IS NOT " a <> "OF " <> TextBuilders.renderInParens (toTextBuilder b)
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
        [ DefaultAExpr <$ Parsers.keyword "default",
          UniqueAExpr <$> (Parsers.keyword "unique" *> Parsers.space1 *> parser),
          Parsers.qualOpExpr aExpr PrefixQualOpAExpr,
          PlusAExpr <$> Parsers.plusedExpr aExpr,
          MinusAExpr <$> Parsers.minusedExpr aExpr,
          NotAExpr <$> (Parsers.keyword "not" *> Parsers.space1 *> aExpr),
          CExprAExpr <$> cExpr
        ]
    suffix a =
      asum
        [ overlapsSuffix a,
          do
            Parsers.space1
            b <- Parser.wrapToHead parser
            Parsers.space1
            c <- Parser.wrapToHead parser
            Parsers.space
            d <- Left <$> Parser.wrapToHead parser <|> Right <$> Parsers.inParens aExpr
            return (SubqueryAExpr a b c d),
          Parsers.typecastExpr a TypecastAExpr,
          CollateAExpr a <$> (Parsers.space1 *> Parsers.keyword "collate" *> Parsers.space1 *> Parser.endHead *> parser),
          AtTimeZoneAExpr a <$> (Parsers.space1 *> Parsers.keyphrase "at time zone" *> Parsers.space1 *> Parser.endHead *> aExpr),
          Parsers.symbolicBinOpExpr a aExpr SymbolicBinOpAExpr,
          SuffixQualOpAExpr a <$> (Parsers.space *> parser),
          AndAExpr a <$> (Parsers.space1 *> Parsers.keyword "and" *> Parsers.space1 *> Parser.endHead *> aExpr),
          OrAExpr a <$> (Parsers.space1 *> Parsers.keyword "or" *> Parsers.space1 *> Parser.endHead *> aExpr),
          do
            Parsers.space1
            b <- Parsers.trueIfPresent (Parsers.keyword "not" *> Parsers.space1)
            c <- parser
            Parsers.space1
            Parser.endHead
            d <- aExpr
            e <- optional (Parsers.space1 *> Parsers.keyword "escape" *> Parsers.space1 *> Parser.endHead *> aExpr)
            return (VerbalExprBinOpAExpr a b c d e),
          do
            Parsers.space1
            Parsers.keyword "is"
            Parsers.space1
            Parser.endHead
            b <- Parsers.trueIfPresent (Parsers.keyword "not" *> Parsers.space1)
            c <-
              asum
                [ NullAExprReversableOp <$ Parsers.keyword "null",
                  TrueAExprReversableOp <$ Parsers.keyword "true",
                  FalseAExprReversableOp <$ Parsers.keyword "false",
                  UnknownAExprReversableOp <$ Parsers.keyword "unknown",
                  DistinctFromAExprReversableOp <$> (Parsers.keyword "distinct" *> Parsers.space1 *> Parsers.keyword "from" *> Parsers.space1 *> Parser.endHead *> aExpr),
                  OfAExprReversableOp <$> (Parsers.keyword "of" *> Parsers.space1 *> Parser.endHead *> Parsers.inParens parser),
                  DocumentAExprReversableOp <$ Parsers.keyword "document"
                ]
            return (ReversableOpAExpr a b c),
          do
            Parsers.space1
            b <- Parsers.trueIfPresent (Parsers.keyword "not" *> Parsers.space1)
            Parsers.keyword "between"
            Parsers.space1
            Parser.endHead
            c <-
              asum
                [ BetweenSymmetricAExprReversableOp <$ (Parsers.keyword "symmetric" *> Parsers.space1),
                  BetweenAExprReversableOp True <$ (Parsers.keyword "asymmetric" *> Parsers.space1),
                  pure (BetweenAExprReversableOp False)
                ]
            d <- parser
            Parsers.space1
            Parsers.keyword "and"
            Parsers.space1
            e <- aExpr
            return (ReversableOpAExpr a b (c d e)),
          do
            Parsers.space1
            b <- Parsers.trueIfPresent (Parsers.keyword "not" *> Parsers.space1)
            Parsers.keyword "in"
            Parsers.space
            c <- InAExprReversableOp <$> parser
            return (ReversableOpAExpr a b c),
          IsnullAExpr a <$ (Parsers.space1 *> Parsers.keyword "isnull"),
          NotnullAExpr a <$ (Parsers.space1 *> Parsers.keyword "notnull")
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
  Parsers.space1
  Parsers.keyword "overlaps"
  Parser.endHead
  Parsers.space1
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
-- Parsers.keyword (@USING@\/@ASC@\/@DESC@\/@NULLS@) that is meant to terminate it.
filteredParser :: [Text] -> Parser AExpr
filteredParser excluded = customizedParser (CExpr.customizedParser (Parsers.filteredColIdLike UnquotedIdent parser excluded))

-- |
-- Whether the given 'AExpr' is safe to place in the left\/accumulator
-- position of a suffix production without parenthesizing it — see
-- @renderOperand@ in the 'IsAst' instance above for why that position is
-- special. A shape is bounded when parsing it can never end in an
-- unrestricted recursive @a_expr@ call, i.e. control is guaranteed to
-- return to @suffixRec@'s loop once it's done, __and__ nothing that could
-- follow (a Parsers.keyword introducing the next suffix, e.g. @AT@ of @AT TIME
-- ZONE@, or an @ESCAPE@ clause external to this value entirely) could be
-- mistaken for the start of a fresh operand.
--
-- 'SuffixQualOpAExpr' fails that second half: since its trailing @qual_Op@
-- has no operand of its own yet, @suffixRec@'s loop tries
-- @symbolicBinOpExpr@ again on whatever follows — and if that's a bare word
-- that isn't reserved (like @AT@), it gets swallowed as this "postfix"
-- operator's operand instead of being left for the Parsers.keyword that was
-- actually meant to follow. E.g. @x +# AT TIME ZONE y@, meant as
-- @AtTimeZoneAExpr (SuffixQualOpAExpr x (+#)) y@, reparses instead as
-- @SymbolicBinOpAExpr x (+#) (CExprAExpr (ColumnrefCExpr "AT"))@ followed by
-- leftover, unparseable @TIME ZONE y@.
isBoundedAExprOperand :: AExpr -> Bool
isBoundedAExprOperand = \case
  PlusAExpr {} -> False
  MinusAExpr {} -> False
  NotAExpr {} -> False
  PrefixQualOpAExpr {} -> False
  SuffixQualOpAExpr {} -> False
  AtTimeZoneAExpr {} -> False
  SymbolicBinOpAExpr {} -> False
  AndAExpr {} -> False
  OrAExpr {} -> False
  VerbalExprBinOpAExpr {} -> False
  ReversableOpAExpr _ _ c -> case c of
    DistinctFromAExprReversableOp {} -> False
    BetweenAExprReversableOp {} -> False
    BetweenSymmetricAExprReversableOp {} -> False
    _ -> True
  _ -> True

-- |
-- A generator for the left\/accumulator position of a suffix production
-- (see 'isBoundedAExprOperand'): whatever the given generator produces gets
-- wrapped in an explicit @'(' a_expr ')'@ (via 'CExpr.InParensCExpr') when
-- it wouldn't otherwise be reachable there, so the result always round-trips
-- through 'parser'.
safeAExprOperand :: Qc.Gen AExpr -> Qc.Gen AExpr
safeAExprOperand gen = do
  a <- gen
  pure $
    if isBoundedAExprOperand a
      then a
      else CExprAExpr (CExpr.InParensCExpr a Nothing)

instance Qc.Arbitrary AExpr where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.sized $ \n ->
      if n <= 1
        then pure DefaultAExpr
        else
          Qc.scale (`div` 2) $
            Qc.oneof
              [ CExprAExpr <$> Qc.arbitrary,
                pure DefaultAExpr,
                TypecastAExpr <$> safeAExprOperand Qc.arbitrary <*> Qc.arbitrary,
                CollateAExpr <$> safeAExprOperand Qc.arbitrary <*> Qc.arbitrary,
                AtTimeZoneAExpr <$> safeAExprOperand Qc.arbitrary <*> Qc.arbitrary,
                PlusAExpr <$> Qc.arbitrary,
                MinusAExpr <$> Qc.arbitrary,
                SymbolicBinOpAExpr <$> safeAExprOperand Qc.arbitrary <*> Qc.arbitrary <*> Qc.arbitrary,
                PrefixQualOpAExpr <$> Qc.arbitrary <*> Qc.arbitrary,
                SuffixQualOpAExpr <$> safeAExprOperand Qc.arbitrary <*> Qc.arbitrary,
                AndAExpr <$> safeAExprOperand Qc.arbitrary <*> Qc.arbitrary,
                OrAExpr <$> safeAExprOperand Qc.arbitrary <*> Qc.arbitrary,
                NotAExpr <$> Qc.arbitrary,
                ( do
                    a <- safeAExprOperand (Qc.scale (`div` 2) Qc.arbitrary)
                    b <- Qc.arbitrary
                    c <- Qc.arbitrary
                    e <- Qc.terminatingMaybe Qc.arbitrary
                    -- See @renderVerbalRhs@ in the 'IsAst' instance above:
                    -- whenever there's an escape clause, @d@ must be
                    -- parenthesized up front so the generated value already
                    -- matches what parsing the (necessarily parenthesized)
                    -- rendering reconstructs.
                    d <- case e of
                      Nothing -> Qc.scale (`div` 2) Qc.arbitrary
                      Just _ -> (\x -> CExprAExpr (CExpr.InParensCExpr x Nothing)) <$> Qc.scale (`div` 2) Qc.arbitrary
                    pure (VerbalExprBinOpAExpr a b c d e)
                ),
                ReversableOpAExpr <$> safeAExprOperand Qc.arbitrary <*> Qc.arbitrary <*> Qc.arbitrary,
                IsnullAExpr <$> safeAExprOperand Qc.arbitrary,
                NotnullAExpr <$> safeAExprOperand Qc.arbitrary,
                OverlapsAExpr <$> Qc.arbitrary <*> Qc.arbitrary,
                SubqueryAExpr <$> safeAExprOperand (Qc.scale (`div` 2) Qc.arbitrary) <*> Qc.arbitrary <*> Qc.arbitrary <*> Qc.scale (`div` 2) Qc.arbitrary,
                UniqueAExpr <$> Qc.arbitrary
              ]
