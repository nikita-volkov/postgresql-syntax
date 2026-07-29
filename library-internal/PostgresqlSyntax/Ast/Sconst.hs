module PostgresqlSyntax.Ast.Sconst where

import qualified Data.Text as Text
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.Shrinks as Shrinks
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
  toTextBuilder settings (Sconst a) = "'" <> TextBuilder.text (Text.replace "'" "''" a) <> "'"
  parser settings = Sconst <$> (Parsers.quotedString '\'' <|> Parsers.dollarQuotedSconst)

instance Qc.Arbitrary Sconst where
  shrink (Sconst a) = Sconst <$> Shrinks.text a
  arbitrary = do
    len <- Qc.sized (\n -> Qc.choose (0, min 1000 (n * 20)))
    Sconst . Text.pack <$> Qc.vectorOf len (Qc.arbitrary `Qc.suchThat` (not . isControl))
