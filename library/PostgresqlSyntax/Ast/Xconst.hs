module PostgresqlSyntax.Ast.Xconst where

import qualified Data.Text as Text
import HeadedMegaparsec
import PostgresqlSyntax.Extras.HeadedMegaparsec hiding (run)
import PostgresqlSyntax.IsAst
import qualified PostgresqlSyntax.Predicate as Predicate
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import TextBuilder (text)

-- |
-- ==== References
-- @
-- XCONST
-- @
newtype Xconst = Xconst Text
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst Xconst where
  toTextBuilder (Xconst a) = "X'" <> text a <> "'"
  parser = label "hex literal" $ do
    string' "x'"
    endHead
    a <- takeWhile1P (Just "Hex digit") Predicate.hexDigit
    char '\''
    return (Xconst a)

instance Arbitrary Xconst where
  arbitrary = do
    len <- choose (1, 100)
    Xconst . Text.pack <$> vectorOf len (elements "0123456789abcdefABCDEF")
