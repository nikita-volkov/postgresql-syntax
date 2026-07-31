module PostgresqlSyntax.Ast.AExpr
  ( AExpr (..),
  )
where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Algebra
import PostgresqlSyntax.Ast.AExprReversableOp
import PostgresqlSyntax.Ast.AnyName
import PostgresqlSyntax.Ast.CExpr (CExpr)
import qualified PostgresqlSyntax.Ast.CExpr as CExpr
import PostgresqlSyntax.Ast.QualOp
import PostgresqlSyntax.Ast.Row
import {-# SOURCE #-} PostgresqlSyntax.Ast.SelectWithParens (SelectWithParens)
import PostgresqlSyntax.Ast.SubType
import PostgresqlSyntax.Ast.SubqueryOp
import PostgresqlSyntax.Ast.SymbolicExprBinOp
import PostgresqlSyntax.Ast.Typename
import PostgresqlSyntax.Ast.VerbalExprBinOp
import qualified PostgresqlSyntax.Helpers.Gens as Gens
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
import PostgresqlSyntax.Prelude
import PostgresqlSyntax.Settings (Settings)
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
  toTextBuilder settings = \case
    CExprAExpr a -> toTextBuilder settings a
    TypecastAExpr a b -> renderOperand a <> " :: " <> toTextBuilder settings b
    CollateAExpr a b -> renderOperand a <> " COLLATE " <> toTextBuilder settings b
    AtTimeZoneAExpr a b -> renderOperand a <> " AT TIME ZONE " <> toTextBuilder settings b
    PlusAExpr a -> "+ " <> toTextBuilder settings a
    MinusAExpr a -> "- " <> toTextBuilder settings a
    SymbolicBinOpAExpr a b c -> renderOperand a <> " " <> toTextBuilder settings b <> " " <> toTextBuilder settings c
    PrefixQualOpAExpr a b -> toTextBuilder settings a <> " " <> toTextBuilder settings b
    AndAExpr a b -> renderOperand a <> " AND " <> toTextBuilder settings b
    OrAExpr a b -> renderOperand a <> " OR " <> toTextBuilder settings b
    NotAExpr a -> "NOT " <> toTextBuilder settings a
    VerbalExprBinOpAExpr a b c d e -> renderOperand a <> " " <> bool "" "NOT " b <> toTextBuilder settings c <> " " <> renderVerbalRhs e d <> foldMap (mappend " ESCAPE " . toTextBuilder settings) e
    ReversableOpAExpr a b c -> renderOperand a <> " " <> renderAExprReversableOp b c
    IsnullAExpr a -> renderOperand a <> " ISNULL"
    NotnullAExpr a -> renderOperand a <> " NOTNULL"
    OverlapsAExpr a b -> toTextBuilder settings a <> " OVERLAPS " <> toTextBuilder settings b
    SubqueryAExpr a b c d -> renderOperand a <> " " <> toTextBuilder settings b <> " " <> toTextBuilder settings c <> " " <> either (toTextBuilder settings) (TextBuilders.renderInParens . toTextBuilder settings) d
    UniqueAExpr a -> "UNIQUE " <> toTextBuilder settings a
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
        | isBoundedAExprOperand a = toTextBuilder settings a
        | otherwise = TextBuilders.renderInParens (toTextBuilder settings a)
      -- The @d@ operand of a @LIKE@\/@ILIKE@\/@SIMILAR TO@ production, when
      -- it has a trailing @ESCAPE@ clause of its own (@e@). Since @d@ is
      -- parsed via an unrestricted recursive @a_expr@, rendering it bare
      -- when it's unbounded (see 'isBoundedAExprOperand') — e.g. a dangling
      -- operator, or an escape-less 'VerbalExprBinOpAExpr' whose own greedy
      -- @optional escape@ check would otherwise swallow the text meant for
      -- \*this* production's @ESCAPE@ — reparses differently. Bounded shapes
      -- (a column reference, a constant, an already-parenthesized
      -- expression, DEFAULT, …) always terminate cleanly and never need the
      -- extra parens, exactly like @renderOperand@'s left position.
      renderVerbalRhs e d = case e of
        Nothing -> toTextBuilder settings d
        Just _
          | isBoundedAExprOperand d -> toTextBuilder settings d
          | otherwise -> TextBuilders.renderInParens (toTextBuilder settings d)
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
        DistinctFromAExprReversableOp b -> bool "IS " "IS NOT " a <> "DISTINCT FROM " <> toTextBuilder settings b
        OfAExprReversableOp b -> bool "IS " "IS NOT " a <> "OF " <> TextBuilders.renderInParens (toTextBuilder settings b)
        BetweenAExprReversableOp b c d -> bool "" "NOT " a <> bool "BETWEEN " "BETWEEN ASYMMETRIC " b <> toTextBuilder settings c <> " AND " <> toTextBuilder settings d
        BetweenSymmetricAExprReversableOp b c -> bool "" "NOT " a <> "BETWEEN SYMMETRIC " <> toTextBuilder settings b <> " AND " <> toTextBuilder settings c
        InAExprReversableOp b -> bool "" "NOT " a <> "IN " <> toTextBuilder settings b
        DocumentAExprReversableOp -> bool "IS " "IS NOT " a <> "DOCUMENT"
  parser settings = customizedParser settings (parser settings)

-- |
-- Parameterized over the 'PostgresqlSyntax.Ast.CExpr' parser embedded at the
-- base case — the only axis 'filteredParser' needs to customize (via
-- 'PostgresqlSyntax.Ast.CExpr.customizedParser'). Every other occurrence of
-- @a_expr@\/@b_expr@\/@select_with_parens@ in the grammar below uses the
-- ordinary, unfiltered parsers, exactly as the pre-extraction
-- @customizedAExpr@ did (its @bExpr@\/@selectWithParens@ references were
-- never threaded through the @cExpr@ parameter either).
customizedParser :: Settings -> Parser CExpr -> Parser AExpr
customizedParser settings cExpr = suffixRec base suffix
  where
    aExpr = customizedParser settings cExpr
    base =
      asum
        [ DefaultAExpr <$ Parsers.keyword "default",
          UniqueAExpr <$> (Parsers.keyword "unique" *> Parsers.space1 *> parser settings),
          Parsers.qualOpExpr settings aExpr PrefixQualOpAExpr,
          PlusAExpr <$> Parsers.plusedExpr aExpr,
          MinusAExpr <$> Parsers.minusedExpr aExpr,
          NotAExpr <$> (Parsers.keyword "not" *> Parsers.space1 *> aExpr),
          CExprAExpr <$> cExpr
        ]
    suffix a =
      asum
        [ overlapsSuffix settings a,
          do
            Parsers.space1
            b <- Parser.wrapToHead (parser settings)
            Parsers.space1
            c <- Parser.wrapToHead (parser settings)
            Parsers.space
            d <- Left <$> Parser.wrapToHead (parser settings) <|> Right <$> Parsers.inParens aExpr
            return (SubqueryAExpr a b c d),
          Parsers.typecastExpr settings a TypecastAExpr,
          CollateAExpr a <$> (Parsers.space1 *> Parsers.keyword "collate" *> Parsers.space1 *> Parser.endHead *> parser settings),
          AtTimeZoneAExpr a <$> (Parsers.space1 *> Parsers.keyphrase "at time zone" *> Parsers.space1 *> Parser.endHead *> aExpr),
          Parsers.symbolicBinOpExpr settings a aExpr SymbolicBinOpAExpr,
          AndAExpr a <$> (Parsers.space1 *> Parsers.keyword "and" *> Parsers.space1 *> Parser.endHead *> aExpr),
          OrAExpr a <$> (Parsers.space1 *> Parsers.keyword "or" *> Parsers.space1 *> Parser.endHead *> aExpr),
          do
            Parsers.space1
            b <- Parsers.trueIfPresent (Parsers.keyword "not" *> Parsers.space1)
            c <- parser settings
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
                  OfAExprReversableOp <$> (Parsers.keyword "of" *> Parsers.space1 *> Parser.endHead *> Parsers.inParens (parser settings)),
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
            d <- parser settings
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
            c <- InAExprReversableOp <$> parser settings
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
overlapsSuffix :: Settings -> AExpr -> Parser AExpr
overlapsSuffix settings a = do
  b <- maybe empty pure (aExprRow a)
  Parsers.space1
  Parsers.keyword "overlaps"
  Parser.endHead
  Parsers.space1
  c <- parser settings
  return (OverlapsAExpr b c)
  where
    aExprRow = \case
      CExprAExpr (CExpr.ExplicitRowCExpr x) -> Just (ExplicitRowRow x)
      CExprAExpr (CExpr.ImplicitRowCExpr x) -> Just (ImplicitRowRow x)
      _ -> Nothing

-- |
-- Whether the given 'AExpr' is safe to place in the left\/accumulator
-- position of a suffix production without parenthesizing it — see
-- @renderOperand@ in the 'IsAst' instance above for why that position is
-- special. A shape is bounded when parsing it can never end in an
-- unrestricted recursive @a_expr@ call, i.e. control is guaranteed to
-- return to @suffixRec@'s loop once it's done.
--
-- Note this predicate is purely about operator precedence and
-- associativity: an unbounded shape is one whose own rendering ends in an
-- unrestricted recursive @a_expr@, so placed bare in the left position it
-- would re-absorb the suffix that follows (e.g. rendering
-- @SymbolicBinOpAExpr (NotAExpr x) op y@ plainly as @NOT x op y@ reparses
-- as @NotAExpr (SymbolicBinOpAExpr x op y)@). It is /not/ a general
-- terminator-keyword guard — it isn't a general-purpose mechanism against
-- an expression swallowing a keyword that terminates an enclosing
-- production; the only shape that ever created that hazard by construction
-- was the postfix @a_expr qual_Op@ production, which no longer exists here
-- or in @references/gram.y@ (see gram.y:15985,15987; Postgres removed
-- postfix operators in v14). It does incidentally get reused for that
-- purpose in "PostgresqlSyntax.Ast.TargetEl" (to stop an @OrAExpr@'s right
-- operand from absorbing a following implicit alias) — that's just one
-- call site's use of it, not evidence of general applicability.
isBoundedAExprOperand :: AExpr -> Bool
isBoundedAExprOperand = \case
  PlusAExpr {} -> False
  MinusAExpr {} -> False
  NotAExpr {} -> False
  PrefixQualOpAExpr {} -> False
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

instance Refines SelectWithParens AExpr where
  embed a = CExprAExpr (CExpr.SelectWithParensCExpr a Nothing)
  project = \case
    CExprAExpr (CExpr.SelectWithParensCExpr a Nothing) -> Just a
    _ -> Nothing

-- |
-- Collapses the non-canonical @Right@-wrapping-a-bare-@select_with_parens@
-- shape of 'SubqueryAExpr'\'s final field to the @Left@ shape the parser
-- actually produces for it.
--
-- @a_expr subquery_Op sub_type select_with_parens@ (i.e. @Left@) and
-- @a_expr subquery_Op sub_type '(' a_expr ')'@ (i.e. @Right@) overlap
-- whenever the parenthesized @a_expr@ is itself nothing but a bare,
-- indirection-less @select_with_parens@: both parse @x = ANY ((select 1))@.
-- @suffix@ tries the @select_with_parens@ alternative before the
-- parenthesized @a_expr@ one (see the @d <- Left <$> ... <|> Right <$> ...@
-- line above), so that's always what the parser returns — never @Right@ —
-- making the latter non-canonical for this shape. Both 'arbitrary' and
-- 'shrink' can otherwise construct it, which renders fine but parses back to
-- a different, canonical value and so breaks the roundtrip property.
instance Canonicalizes AExpr where
  canonicalize = \case
    SubqueryAExpr a b c (Right d)
      | Just inner <- project d -> SubqueryAExpr a b c (Left inner)
    other -> other

instance Qc.Arbitrary AExpr where
  shrink = fmap canonicalize . Qc.genericShrink
  arbitrary =
    fmap canonicalize $ Qc.sized $ \n ->
      if n <= 1
        then pure DefaultAExpr
        else
          Qc.oneof
            [ CExprAExpr <$> Qc.arbitrary,
              pure DefaultAExpr,
              TypecastAExpr <$> safeAExprOperand (Gens.downscale Qc.arbitrary) <*> Qc.arbitrary,
              CollateAExpr <$> safeAExprOperand (Gens.downscale Qc.arbitrary) <*> Qc.arbitrary,
              AtTimeZoneAExpr <$> safeAExprOperand (Gens.downscale Qc.arbitrary) <*> Gens.downscale Qc.arbitrary,
              PlusAExpr <$> Gens.downscale Qc.arbitrary,
              MinusAExpr <$> Gens.downscale Qc.arbitrary,
              SymbolicBinOpAExpr <$> safeAExprOperand (Gens.downscale Qc.arbitrary) <*> Qc.arbitrary <*> Gens.downscale Qc.arbitrary,
              PrefixQualOpAExpr <$> Qc.arbitrary <*> Gens.downscale Qc.arbitrary,
              AndAExpr <$> safeAExprOperand (Gens.downscale Qc.arbitrary) <*> Gens.downscale Qc.arbitrary,
              OrAExpr <$> safeAExprOperand (Gens.downscale Qc.arbitrary) <*> Gens.downscale Qc.arbitrary,
              NotAExpr <$> Gens.downscale Qc.arbitrary,
              ( do
                  a <- safeAExprOperand (Gens.downscale Qc.arbitrary)
                  b <- Qc.arbitrary
                  c <- Qc.arbitrary
                  e <- Gens.terminatingMaybe (Gens.downscale Qc.arbitrary)
                  -- See @renderVerbalRhs@ in the 'IsAst' instance above:
                  -- whenever there's an escape clause, an unbounded @d@ must
                  -- be pre-wrapped in parens so the generated value already
                  -- matches what parsing the (necessarily parenthesized)
                  -- rendering reconstructs — the same rule 'safeAExprOperand'
                  -- applies for the left/accumulator position.
                  d <- case e of
                    Nothing -> Gens.downscale Qc.arbitrary
                    Just _ -> safeAExprOperand (Gens.downscale Qc.arbitrary)
                  pure (VerbalExprBinOpAExpr a b c d e)
              ),
              ReversableOpAExpr <$> safeAExprOperand (Gens.downscale Qc.arbitrary) <*> Qc.arbitrary <*> Qc.arbitrary,
              IsnullAExpr <$> safeAExprOperand (Gens.downscale Qc.arbitrary),
              NotnullAExpr <$> safeAExprOperand (Gens.downscale Qc.arbitrary),
              OverlapsAExpr <$> Qc.arbitrary <*> Qc.arbitrary,
              SubqueryAExpr <$> safeAExprOperand (Gens.downscale Qc.arbitrary) <*> Qc.arbitrary <*> Qc.arbitrary <*> Gens.downscale Qc.arbitrary,
              UniqueAExpr <$> Gens.downscale Qc.arbitrary
            ]
