module PostgresqlSyntax.Ast.Fconst where

import PostgresqlSyntax.Extras.HeadedMegaparsec (float)
import PostgresqlSyntax.Extras.TextBuilder (doubleDec)
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude
import Test.QuickCheck (suchThat)

-- |
-- ==== References
-- @
-- FCONST
-- @
newtype Fconst = Fconst Double
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst Fconst where
  toTextBuilder (Fconst a) = doubleDec a
  parser = Fconst <$> float

instance Arbitrary Fconst where
  arbitrary =
    Fconst
      <$> (arbitrary `suchThat` (\a -> fromIntegral (round a :: Int) /= a))
