module PostgresqlSyntax.Ast.Bconst where

import qualified Data.Text as Text
import HeadedMegaparsec hiding (string)
import PostgresqlSyntax.Extras.HeadedMegaparsec hiding (run)
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, option, some, try)
import TextBuilder (text)

-- |
-- ==== References
-- @
-- BCONST
-- @
newtype Bconst = Bconst Text
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst Bconst where
  toTextBuilder (Bconst a) = "B'" <> text a <> "'"
  parser = label "bit literal" $ do
    string' "b'"
    endHead
    a <- takeWhile1P (Just "0 or 1") (\b -> b == '0' || b == '1')
    char '\''
    return (Bconst a)

instance Arbitrary Bconst where
  arbitrary = do
    len <- choose (1, 100)
    Bconst . Text.pack <$> vectorOf len (elements "01")
