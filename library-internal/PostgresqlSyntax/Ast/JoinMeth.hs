module PostgresqlSyntax.Ast.JoinMeth where

import PostgresqlSyntax.Algebra
import PostgresqlSyntax.Ast.JoinQual
import PostgresqlSyntax.Ast.JoinType
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
import PostgresqlSyntax.Prelude
import qualified Test.QuickCheck as Qc

-- |
-- ==== References
-- @
-- | table_ref CROSS JOIN table_ref
-- | table_ref join_type JOIN table_ref join_qual
-- | table_ref JOIN table_ref join_qual
-- | table_ref NATURAL join_type JOIN table_ref
-- | table_ref NATURAL JOIN table_ref
-- @
--
-- In the concrete grammar, a 'JoinQual' is only ever written /after/ the
-- second @table_ref@ — never contiguous with the rest of this type's own
-- text. 'PostgresqlSyntax.Ast.TableRef' (the recursion hub this lives
-- beside) therefore does its own hand-written interleaving of table refs
-- and join methods rather than calling this instance; this one exists so
-- 'JoinMeth' has a sensible, self-contained, round-trippable grammar in
-- isolation (e.g. for 'Arbitrary'), not because anything threads through
-- it.
data JoinMeth
  = CrossJoinMeth
  | QualJoinMeth (Maybe JoinType) JoinQual
  | NaturalJoinMeth (Maybe JoinType)
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst JoinMeth where
  toTextBuilder settings = \case
    CrossJoinMeth -> "CROSS JOIN"
    QualJoinMeth a b -> TextBuilders.optLexemes [fmap (toTextBuilder settings) a, Just "JOIN", Just (toTextBuilder settings b)]
    NaturalJoinMeth a -> TextBuilders.optLexemes [Just "NATURAL", fmap (toTextBuilder settings) a, Just "JOIN"]
  parser settings =
    asum
      [ CrossJoinMeth <$ Parsers.keyphrase "cross join",
        do
          a <- optional (parser settings <* Parsers.space1)
          Parsers.keyword "join"
          Parsers.space1
          b <- parser settings
          return (QualJoinMeth a b),
        do
          Parsers.keyword "natural"
          Parsers.space1
          a <- optional (parser settings <* Parsers.space1)
          Parsers.keyword "join"
          return (NaturalJoinMeth a)
      ]

instance Qc.Arbitrary JoinMeth where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.oneof
      [ pure CrossJoinMeth,
        QualJoinMeth <$> Qc.arbitrary <*> Qc.arbitrary,
        NaturalJoinMeth <$> Qc.arbitrary
      ]
