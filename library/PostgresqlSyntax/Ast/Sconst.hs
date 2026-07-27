module PostgresqlSyntax.Ast.Sconst where

import qualified Data.Text as Text
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, option, some, try)
import Test.QuickCheck (suchThat)
import TextBuilder (text)

-- |
-- ==== References
-- @
-- Sconst
-- @
newtype Sconst = Sconst Text
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst Sconst where
  toTextBuilder (Sconst a) = "'" <> text (Text.replace "'" "''" a) <> "'"
  parser = Sconst <$> (quotedString '\'' <|> dollarQuotedSconst)

instance Arbitrary Sconst where
  arbitrary = do
    len <- sized (\n -> choose (0, min 1000 (n * 20)))
    Sconst . Text.pack <$> vectorOf len (arbitrary `suchThat` (not . isControl))
