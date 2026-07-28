module PostgresqlSyntax.Ast.VerbalExprBinOp where

import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude
import qualified Test.QuickCheck as Qc

-- |
-- ==== References
-- @
--   | LIKE
--   | NOT_LA LIKE
--   | ILIKE
--   | NOT_LA ILIKE
--   | SIMILAR TO
--   | NOT_LA SIMILAR TO
-- @
data VerbalExprBinOp
  = LikeVerbalExprBinOp
  | IlikeVerbalExprBinOp
  | SimilarToVerbalExprBinOp
  deriving (Show, Generic, Eq, Ord, Data, Enum, Bounded)

instance IsAst VerbalExprBinOp where
  toTextBuilder = \case
    LikeVerbalExprBinOp -> "LIKE"
    IlikeVerbalExprBinOp -> "ILIKE"
    SimilarToVerbalExprBinOp -> "SIMILAR TO"
  parser =
    asum
      [ LikeVerbalExprBinOp <$ Parsers.keyword "like",
        IlikeVerbalExprBinOp <$ Parsers.keyword "ilike",
        SimilarToVerbalExprBinOp <$ Parsers.keyphrase "similar to"
      ]

instance Qc.Arbitrary VerbalExprBinOp where
  shrink = Qc.genericShrink
  arbitrary = Qc.elements [minBound .. maxBound]
