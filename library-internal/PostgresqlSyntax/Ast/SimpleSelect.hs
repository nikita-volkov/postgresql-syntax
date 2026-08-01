module PostgresqlSyntax.Ast.SimpleSelect
  ( SimpleSelect (..),
    SelectChainLink,
  )
where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Algebra
import PostgresqlSyntax.Ast.FromClause
import PostgresqlSyntax.Ast.GroupClause
import PostgresqlSyntax.Ast.HavingClause
import PostgresqlSyntax.Ast.IntoClause
import PostgresqlSyntax.Ast.RelationExpr
import PostgresqlSyntax.Ast.SelectBinOp
import PostgresqlSyntax.Ast.SelectClause
import PostgresqlSyntax.Ast.Targeting
import PostgresqlSyntax.Ast.ValuesClause
import PostgresqlSyntax.Ast.WhereClause
import PostgresqlSyntax.Ast.WindowClause
import qualified PostgresqlSyntax.Helpers.Gens as Gens
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
import PostgresqlSyntax.Prelude
import PostgresqlSyntax.Settings (Settings)
import qualified Test.QuickCheck as Qc

-- |
-- ==== References
-- @
-- simple_select:
--   |  SELECT opt_all_clause opt_target_list
--       into_clause from_clause where_clause
--       group_clause having_clause window_clause
--   |  SELECT distinct_clause target_list
--       into_clause from_clause where_clause
--       group_clause having_clause window_clause
--   |  values_clause
--   |  TABLE relation_expr
--   |  select_clause UNION all_or_distinct select_clause
--   |  select_clause INTERSECT all_or_distinct select_clause
--   |  select_clause EXCEPT all_or_distinct select_clause
-- @
--
-- Hosts the real @select_clause@ grammar (including its
-- @UNION@\/@INTERSECT@\/@EXCEPT@-chaining) for both itself and
-- "PostgresqlSyntax.Ast.SelectNoParens", which shares it — see
-- 'PostgresqlSyntax.Ast.SelectClause'\'s module documentation for why.
data SimpleSelect
  = NormalSimpleSelect (Maybe Targeting) (Maybe IntoClause) (Maybe FromClause) (Maybe WhereClause) (Maybe GroupClause) (Maybe HavingClause) (Maybe WindowClause)
  | ValuesSimpleSelect ValuesClause
  | TableSimpleSelect RelationExpr
  | BinSimpleSelect SelectBinOp SelectClause (Maybe Bool) SelectClause
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst SimpleSelect where
  toTextBuilder settings = \case
    NormalSimpleSelect a b c d e f g ->
      TextBuilders.optLexemes
        [ Just "SELECT",
          fmap (toTextBuilder settings) a,
          fmap (toTextBuilder settings) b,
          fmap (toTextBuilder settings) c,
          fmap (toTextBuilder settings) d,
          fmap (toTextBuilder settings) e,
          fmap (toTextBuilder settings) f,
          fmap (toTextBuilder settings) g
        ]
    ValuesSimpleSelect a -> toTextBuilder settings a
    TableSimpleSelect a -> "TABLE " <> toTextBuilder settings a
    BinSimpleSelect a b c d -> toTextBuilder settings b <> " " <> toTextBuilder settings a <> foldMap (mappend " " . TextBuilders.renderAllOrDistinct) c <> " " <> toTextBuilder settings d

  -- ==== Law
  --
  -- @parser = parseExtended \@SelectClause \<|\> baseSimpleSelect@ — a bare
  -- 'SimpleSelect' is either a @select_clause@ chain of at least one
  -- @UNION@\/@INTERSECT@\/@EXCEPT@ (see 'LeftRecursion' below), or, failing
  -- that (no continuation follows), one of the non-chain base cases; it's
  -- never a bare, zero-extension @select_clause@ (that's not a
  -- 'SimpleSelect' at all — see 'SelectClause'). The chain alternative has
  -- to come first: trying 'baseSimpleSelect' alone would succeed on just
  -- the head of a chain and never look for what follows.
  parser settings = parseExtended @SelectClause settings <|> nonRecursiveParser settings

-- |
-- The @simple_select@ productions that don't left-recurse through
-- @select_clause@ — i.e. everything but @select_clause BINOP
-- select_clause@. "PostgresqlSyntax.Ast.SelectClause" reaches these
-- through this class method, which is why 'SimpleSelect' needs no helper
-- export.
instance LeftRecursive SimpleSelect where
  nonRecursiveParser settings =
    asum
      [ do
          Parsers.keyword "select"
          Parsers.notFollowedBy $ Parsers.satisfy isAlphaNum
          Parser.endHead
          targeting <- optional (Parsers.space1 *> parser settings)
          intoClause <- optional (Parsers.space1 *> parser settings)
          fromClause <- optional (Parsers.space1 *> parser settings)
          whereClause <- optional (Parsers.space1 *> parser settings)
          groupClause <- optional (Parsers.space1 *> parser settings)
          havingClause <- optional (Parsers.space1 *> parser settings)
          windowClause <- optional (Parsers.space1 *> parser settings)
          return (NormalSimpleSelect targeting intoClause fromClause whereClause groupClause havingClause windowClause),
        do
          Parsers.keyword "table"
          Parsers.space1
          Parser.endHead
          TableSimpleSelect <$> parser settings,
        ValuesSimpleSelect <$> parser settings
      ]

-- |
-- The left-recursion-eliminated form of @select_clause@: a bare
-- 'SelectClause' (either a parenthesized select, or one of
-- 'SimpleSelect'\'s non-chain cases) is the non-recursive base (@β@),
-- and each @UNION@\/@INTERSECT@\/@EXCEPT@ continuation (@α@) is a
-- 'SelectBinOp' plus its @ALL@\/@DISTINCT@ qualifier and right operand,
-- applied via 'BinSimpleSelect'.
--
-- 'foldExtensions' is overridden because, unlike
-- "PostgresqlSyntax.Ast.TableRef"\'s join chain, this hub's items aren't
-- all one precedence level: @gram.y@ declares @%left UNION EXCEPT@ before
-- (i.e. binding looser than) @%left INTERSECT@ (gram.y:813-814), both
-- left-associative. The default fold (uniform left-association) would
-- root @a INTERSECT b UNION c@ at @INTERSECT@ and nest @a EXCEPT b EXCEPT
-- c@ to the right — both wrong.
instance LeftRecursion SelectClause SimpleSelect SelectChainLink where
  extension settings = do
    op <- Parsers.space1 *> parser settings <* Parsers.space1
    Parser.endHead
    distinct <- optional (Parsers.allOrDistinct <* Parsers.space1)
    rhs <- nonRecursiveParser @SelectClause settings
    return (SelectChainLink op distinct rhs)

  applyExtension lhs (SelectChainLink op distinct rhs) = BinSimpleSelect op lhs distinct rhs

  foldExtensions = foldChain

-- |
-- One @UNION@\/@INTERSECT@\/@EXCEPT@ continuation, minus the left operand
-- it applies to — the 'PostgresqlSyntax.Algebra.LeftRecursion' @item@ for
-- 'SimpleSelect', named so it stops leaking into
-- "PostgresqlSyntax.Ast.SelectClause"\'s @hs-boot@ instance head as an
-- anonymous triple.
data SelectChainLink = SelectChainLink SelectBinOp (Maybe Bool) SelectClause

-- |
-- ==== The precedence fold
--
-- @go@ applies items to the accumulator one at a time, left to right —
-- which by itself is already left-associative for a run of same-operator
-- items, INTERSECT included. What needs help is a /low-precedence/ item
-- (@UNION@\/@EXCEPT@) immediately followed by @INTERSECT@ items: those
-- bind tighter, so they must combine into that item's right operand
-- before @go@ applies it, not become separate steps of @go@'s own fold.
-- @absorbIntersect@ does exactly that — and only that: it leaves an
-- @INTERSECT@ item itself untouched (its rest is handled by @go@'s next
-- iteration, one item at a time), and otherwise absorbs a maximal
-- trailing run of @INTERSECT@ items into the current item's right
-- operand. @go@ then continues from whatever @absorbIntersect@ left
-- unconsumed, and either finishes (if nothing's left — the whole point of
-- ending on 'applyExtension' rather than 'embed' is that the final
-- combination must be the returned @SimpleSelect@, not re-wrapped as a
-- @SelectClause@) or continues.
foldChain :: SelectClause -> NonEmpty SelectChainLink -> SimpleSelect
foldChain base (i0 :| is0) = go base i0 is0
  where
    go acc item rest =
      let (absorbedItem, rest') = absorbIntersect item rest
       in case rest' of
            [] -> applyExtension acc absorbedItem
            item' : rest'' -> go (embed (applyExtension acc absorbedItem)) item' rest''

    absorbIntersect item@(SelectChainLink IntersectSelectBinOp _ _) rest = (item, rest)
    absorbIntersect (SelectChainLink op distinct rhs) (SelectChainLink IntersectSelectBinOp d rhs' : rest) =
      absorbIntersect (SelectChainLink op distinct (SimpleSelectSelectClause (BinSimpleSelect IntersectSelectBinOp rhs d rhs'))) rest
    absorbIntersect item rest = (item, rest)

instance Qc.Arbitrary SimpleSelect where
  shrink = fmap canonicalize . Qc.genericShrink
  arbitrary =
    canonicalize
      <$> Qc.sized
        ( \n ->
            if n <= 1
              then TableSimpleSelect <$> Qc.arbitrary
              else
                Qc.oneof
                  [ NormalSimpleSelect
                      <$> Qc.arbitrary
                      <*> Qc.arbitrary
                      <*> Qc.arbitrary
                      <*> Gens.terminatingMaybe Qc.arbitrary
                      <*> Qc.arbitrary
                      <*> Gens.terminatingMaybe Qc.arbitrary
                      <*> Qc.arbitrary,
                    ValuesSimpleSelect <$> Qc.arbitrary,
                    TableSimpleSelect <$> Qc.arbitrary,
                    BinSimpleSelect <$> Qc.arbitrary <*> Qc.arbitrary <*> Qc.arbitrary <*> Qc.arbitrary
                  ]
        )

-- |
-- Collapses an arbitrary-shaped @BinSimpleSelect@ chain to the shape
-- 'foldChain' actually produces for it (left-associated within each
-- precedence level, @INTERSECT@ binding tighter than @UNION@\/@EXCEPT@ —
-- see 'foldChain' above): 'flattenChain' reduces the chain to its flat
-- sequence of operators and operands regardless of how it's currently
-- nested, and re-folding that sequence with the same 'foldChain' the
-- parser itself uses is by construction the shape @parse . toText@
-- produces. Both 'arbitrary' and 'shrink' can otherwise construct a
-- non-canonical shape, which renders fine but parses back to a
-- different, canonical value and so breaks the roundtrip property.
instance Canonicalizes SimpleSelect where
  canonicalize s@(BinSimpleSelect {}) =
    case flattenChain (SimpleSelectSelectClause s) of
      (headClause, i : is) -> foldChain headClause (i :| is)
      (_, []) -> s
  canonicalize other = other

-- |
-- Reduces a @BinSimpleSelect@ chain, in whatever shape it's currently
-- nested, to its leading operand and the flat, left-to-right sequence of
-- 'SelectChainLink' items that follow it — the inverse of 'foldChain'.
flattenChain :: SelectClause -> (SelectClause, [SelectChainLink])
flattenChain (SimpleSelectSelectClause (BinSimpleSelect op lhs distinct rhs)) =
  let (lhsHead, lhsRest) = flattenChain lhs
      (rhsHead, rhsRest) = flattenChain rhs
   in (lhsHead, lhsRest <> [SelectChainLink op distinct rhsHead] <> rhsRest)
flattenChain c = (c, [])
