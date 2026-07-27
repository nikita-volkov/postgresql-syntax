module PostgresqlSyntax.Ast.JoinMeth where

import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Ast.JoinQual
import PostgresqlSyntax.Ast.JoinType
import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Parser
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
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
  toTextBuilder = \case
    CrossJoinMeth -> "CROSS JOIN"
    QualJoinMeth a b -> optLexemes [fmap toTextBuilder a, Just "JOIN", Just (toTextBuilder b)]
    NaturalJoinMeth a -> optLexemes [Just "NATURAL", fmap toTextBuilder a, Just "JOIN"]
  parser =
    asum
      [ CrossJoinMeth <$ keyphrase "cross join",
        do
          a <- optional (parser <* Parser.space1)
          keyword "join"
          Parser.space1
          b <- parser
          return (QualJoinMeth a b),
        do
          keyword "natural"
          Parser.space1
          a <- optional (parser <* Parser.space1)
          keyword "join"
          return (NaturalJoinMeth a)
      ]

instance Qc.Arbitrary JoinMeth where
  arbitrary =
    Qc.oneof
      [ pure CrossJoinMeth,
        QualJoinMeth <$> Qc.arbitrary <*> Qc.scale (`div` 2) Qc.arbitrary,
        NaturalJoinMeth <$> Qc.arbitrary
      ]
