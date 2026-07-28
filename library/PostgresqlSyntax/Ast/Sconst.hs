module PostgresqlSyntax.Ast.Sconst where

import qualified Data.Text as Text
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import qualified Test.QuickCheck as Qc
import qualified TextBuilder

-- |
-- ==== References
-- @
-- Sconst
-- @
newtype Sconst = Sconst Text
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst Sconst where
  toTextBuilder (Sconst a) = "'" <> TextBuilder.text (Text.replace "'" "''" a) <> "'"
  parser = Sconst <$> (quotedString '\'' <|> dollarQuotedSconst)

instance Qc.Arbitrary Sconst where
  shrink (Sconst a) = Sconst <$> shrinkText a
  arbitrary = do
    len <- Qc.sized (\n -> Qc.choose (0, min 1000 (n * 20)))
    Sconst . Text.pack <$> Qc.vectorOf len (Qc.arbitrary `Qc.suchThat` (not . isControl))
