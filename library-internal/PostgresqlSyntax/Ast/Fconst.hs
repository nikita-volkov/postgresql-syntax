module PostgresqlSyntax.Ast.Fconst where

import qualified PostgresqlSyntax.Extras.TextBuilder as TextBuilder
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
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
  toTextBuilder _settings (Fconst a) = TextBuilder.doubleDec a
  parser _settings = Fconst <$> Parsers.float

instance Qc.Arbitrary Fconst where
  shrink = Qc.genericShrink

  -- Parsed via 'Parsers.float' (unsigned — the sign, when present, is a
  -- separate unary @AExpr@\/@BExpr@ operator applied outside this type), so
  -- it must never be negative, mirroring
  -- 'PostgresqlSyntax.Ast.IntervalSecond'\'s own @nonNegative@.
  arbitrary =
    Fconst . abs
      <$> (Qc.arbitrary `Qc.suchThat` (\a -> fromIntegral (round a :: Int) /= a))
