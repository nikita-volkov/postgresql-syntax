{-# OPTIONS_GHC -Wno-orphans #-}

module PostgresqlSyntax.Ast.JoinedTable
  ( JoinedTable (..),
  )
where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Algebra
import PostgresqlSyntax.Ast.JoinQual
import PostgresqlSyntax.Ast.JoinType
import {-# SOURCE #-} PostgresqlSyntax.Ast.TableRef (TableRef)
import qualified PostgresqlSyntax.Helpers.Gens as Gens
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
import PostgresqlSyntax.Prelude
import qualified Test.QuickCheck as Qc

-- |
-- ==== References
-- @
-- | '(' joined_table ')'
-- | table_ref CROSS JOIN table_ref
-- | table_ref join_type JOIN table_ref join_qual
-- | table_ref JOIN table_ref join_qual
-- | table_ref NATURAL join_type JOIN table_ref
-- | table_ref NATURAL JOIN table_ref
-- @
data JoinedTable
  = InParensJoinedTable JoinedTable
  | CrossJoinedTable TableRef TableRef
  | QualJoinedTable TableRef (Maybe JoinType) TableRef JoinQual
  | NaturalJoinedTable TableRef (Maybe JoinType) TableRef
  deriving (Show, Generic, Eq, Ord, Data)

-- |
-- Parsing delegates to 'parseExtended' over the 'ExtendedBy' instance
-- below — a bare @table_ref@ parse is greedy, absorbing any trailing @CROSS
-- JOIN@\/@JOIN@\/@NATURAL JOIN@ continuation into itself, so a
-- @joined_table@ is never reachable as a bare, zero-extension 'TableRef';
-- it always needs at least one. Failing that, the only remaining
-- @joined_table@ production is the non-left-recursive 'parseBase'.
instance IsAst JoinedTable where
  toTextBuilder settings = \case
    InParensJoinedTable a -> TextBuilders.renderInParens (toTextBuilder settings a)
    CrossJoinedTable a b -> toTextBuilder settings a <> " CROSS JOIN " <> toTextBuilder settings b
    QualJoinedTable a b c d -> toTextBuilder settings a <> TextBuilders.suffixMaybe (toTextBuilder settings) b <> " JOIN " <> toTextBuilder settings c <> " " <> toTextBuilder settings d
    NaturalJoinedTable a b c -> toTextBuilder settings a <> " NATURAL" <> TextBuilders.suffixMaybe (toTextBuilder settings) b <> " JOIN " <> toTextBuilder settings c

  parser settings = parseExtended @TableRef settings <|> parseBase settings

-- |
-- The one @joined_table@ production that doesn't begin with a
-- left-recursive @table_ref@:
--
-- @
--   | '(' joined_table ')'
-- @
--
-- It still recurses — just not on the left, since the opening parenthesis
-- has to be consumed first. "PostgresqlSyntax.Ast.TableRef" reaches it
-- through this class method, which is why 'JoinedTable' needs no helper
-- export.
instance LeftRecursive JoinedTable where
  parseBase settings = InParensJoinedTable <$> Parsers.inParens (parser settings)

instance Qc.Arbitrary JoinedTable where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.sized $ \n ->
      if n <= 1
        then joined
        else Qc.oneof [InParensJoinedTable <$> Gens.downscale Qc.arbitrary, joined]
    where
      joined =
        Qc.oneof
          [ CrossJoinedTable <$> Gens.downscale Qc.arbitrary <*> Gens.downscale Qc.arbitrary,
            QualJoinedTable <$> Gens.downscale Qc.arbitrary <*> Qc.arbitrary <*> Gens.downscale Qc.arbitrary <*> Qc.arbitrary,
            NaturalJoinedTable <$> Gens.downscale Qc.arbitrary <*> Qc.arbitrary <*> Gens.downscale Qc.arbitrary
          ]

-- |
-- The left-recursion-eliminated form of @table_ref@\/@joined_table@: a
-- 'TableRef' is the non-recursive base (@β@, its own 'parseBase'). All
-- three join kinds sit at the same precedence (@%left JOIN CROSS LEFT FULL
-- RIGHT INNER_P NATURAL@ in @gram.y@), and there's nothing to hold between
-- parsing a join and applying it, so no item type is warranted here —
-- unlike "PostgresqlSyntax.Ast.SimpleSelect", this hub isn't collect-then-fold.
instance ExtendedBy TableRef JoinedTable where
  -- ==== References
  -- @
  --   | table_ref CROSS JOIN table_ref
  --   | table_ref join_type JOIN table_ref join_qual
  --   | table_ref JOIN table_ref join_qual
  --   | table_ref NATURAL join_type JOIN table_ref
  --   | table_ref NATURAL JOIN table_ref
  -- @
  --
  -- Parses one join onto 'tr1', then recurses with the built 'JoinedTable'
  -- (embedded back to 'TableRef') as the new left operand, falling back to
  -- what's already built when no further join follows. The
  -- 'Parser.wrapToHead'\/'Parser.endHead' pair around the single join
  -- mirrors 'PostgresqlSyntax.Algebra.parseExtensionChain'\'s per-item
  -- protocol, since this recursive shape can't build on that combinator
  -- directly.
  parseExtensions settings tr1 = do
    built <- Parser.wrapToHead parseOneJoin
    Parser.endHead
    optional (parseExtensions @TableRef settings (embed built)) >>= pure . maybe built id
    where
      parseOneJoin =
        Parsers.space1
          *> asum
            [ do
                Parsers.keyphrase "cross join"
                Parser.endHead
                Parsers.space1
                tr2 <- parseBase @TableRef settings
                return (CrossJoinedTable tr1 tr2),
              do
                jt <- joinTypedJoin
                Parser.endHead
                Parsers.space1
                tr2 <- parser settings
                Parsers.space1
                jq <- parser settings
                return (QualJoinedTable tr1 jt tr2 jq),
              do
                Parsers.keyword "natural"
                Parser.endHead
                Parsers.space1
                jt <- joinTypedJoin
                Parsers.space1
                tr2 <- parseBase @TableRef settings
                return (NaturalJoinedTable tr1 jt tr2)
            ]
      joinTypedJoin =
        Just
          <$> (parser settings <* Parser.endHead <* Parsers.space1 <* Parsers.keyword "join")
            <|> Nothing
          <$ Parsers.keyword "join"
