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
import PostgresqlSyntax.Settings (Settings)
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
-- Parsing delegates to 'parseExtended' over the 'LeftRecursion' instance
-- below — a bare @table_ref@ parse is greedy, absorbing any trailing @CROSS
-- JOIN@\/@JOIN@\/@NATURAL JOIN@ continuation into itself, so a
-- @joined_table@ is never reachable as a bare, zero-extension 'TableRef';
-- it always needs at least one. Failing that, the only remaining
-- @joined_table@ production is the non-left-recursive
-- 'nonRecursiveParser'.
instance IsAst JoinedTable where
  toTextBuilder settings = \case
    InParensJoinedTable a -> TextBuilders.renderInParens (toTextBuilder settings a)
    CrossJoinedTable a b -> toTextBuilder settings a <> " CROSS JOIN " <> toTextBuilder settings b
    QualJoinedTable a b c d -> toTextBuilder settings a <> TextBuilders.suffixMaybe (toTextBuilder settings) b <> " JOIN " <> toTextBuilder settings c <> " " <> toTextBuilder settings d
    NaturalJoinedTable a b c -> toTextBuilder settings a <> " NATURAL" <> TextBuilders.suffixMaybe (toTextBuilder settings) b <> " JOIN " <> toTextBuilder settings c

  parser settings = parseExtended @TableRef settings <|> nonRecursiveParser settings

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
  nonRecursiveParser settings = InParensJoinedTable <$> Parsers.inParens (parser settings)

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
-- A 'JoinedTable' with its left operand removed — the 'item' half of the
-- 'LeftRecursion' instance below. Mirrors 'JoinedTable'\'s three
-- join-bearing constructors one-for-one, each missing its leading
-- 'TableRef'.
data JoinedTableExtension
  = CrossJoinedTableExtension TableRef
  | QualJoinedTableExtension (Maybe JoinType) TableRef JoinQual
  | NaturalJoinedTableExtension (Maybe JoinType) TableRef

-- |
-- The left-recursion-eliminated form of @table_ref@\/@joined_table@: a
-- 'TableRef' is the non-recursive base (@β@, its own
-- 'nonRecursiveParser'), and a @joined_table@ continuation (@α@) is a
-- 'JoinedTableExtension' — one of 'JoinedTable'\'s three join shapes,
-- minus its left operand. All three join kinds sit at the same precedence
-- (@%left JOIN CROSS LEFT FULL RIGHT INNER_P NATURAL@ in @gram.y@), so the
-- default left fold is exactly right — unlike
-- "PostgresqlSyntax.Ast.SimpleSelect", this hub doesn't override
-- 'foldExtensions'.
instance LeftRecursion TableRef JoinedTable JoinedTableExtension where
  -- ==== References
  -- @
  --   | table_ref CROSS JOIN table_ref
  --   | table_ref join_type JOIN table_ref join_qual
  --   | table_ref JOIN table_ref join_qual
  --   | table_ref NATURAL join_type JOIN table_ref
  --   | table_ref NATURAL JOIN table_ref
  -- @
  extension settings =
    Parsers.space1
      *> asum
        [ do
            Parsers.keyphrase "cross join"
            Parser.endHead
            Parsers.space1
            tr2 <- nonRecursiveParser @TableRef settings
            return (CrossJoinedTableExtension tr2),
          do
            jt <- joinTypedJoin
            Parser.endHead
            Parsers.space1
            tr2 <- parser settings
            Parsers.space1
            jq <- parser settings
            return (QualJoinedTableExtension jt tr2 jq),
          do
            Parsers.keyword "natural"
            Parser.endHead
            Parsers.space1
            jt <- joinTypedJoin
            Parsers.space1
            tr2 <- nonRecursiveParser @TableRef settings
            return (NaturalJoinedTableExtension jt tr2)
        ]
    where
      joinTypedJoin =
        Just
          <$> (parser settings <* Parser.endHead <* Parsers.space1 <* Parsers.keyword "join")
            <|> Nothing
          <$ Parsers.keyword "join"

  applyExtension tr1 = \case
    CrossJoinedTableExtension tr2 -> CrossJoinedTable tr1 tr2
    QualJoinedTableExtension jt tr2 jq -> QualJoinedTable tr1 jt tr2 jq
    NaturalJoinedTableExtension jt tr2 -> NaturalJoinedTable tr1 jt tr2
