module PostgresqlSyntax.Ast.Fconst where

import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Parser
import qualified PostgresqlSyntax.Extras.TextBuilder as TextBuilder
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude
import qualified Test.QuickCheck as Qc

-- |
-- ==== References
-- @
-- FCONST
-- @
newtype Fconst = Fconst Double
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst Fconst where
  toTextBuilder (Fconst a) = TextBuilder.doubleDec a
  parser = Fconst <$> Parser.float

instance Qc.Arbitrary Fconst where
  -- \| Parsed via 'Parser.float' (unsigned — the sign, when present, is a
  -- separate unary @AExpr@\/@BExpr@ operator applied outside this type), so
  -- it must never be negative, mirroring
  -- 'PostgresqlSyntax.Ast.IntervalSecond'\'s own @nonNegative@.
  arbitrary =
    Fconst . abs
      <$> (Qc.arbitrary `Qc.suchThat` (\a -> fromIntegral (round a :: Int) /= a))
