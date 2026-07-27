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
  arbitrary =
    Fconst
      <$> (Qc.arbitrary `Qc.suchThat` (\a -> fromIntegral (round a :: Int) /= a))
