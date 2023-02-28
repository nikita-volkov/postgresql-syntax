module PostgresqlSyntax.Predicate where

import qualified Data.HashSet as HashSet
import qualified PostgresqlSyntax.CharSet as CharSet
import qualified PostgresqlSyntax.KeywordSet as KeywordSet
import PostgresqlSyntax.Prelude hiding (expression)

-- * Generic

-- |
-- >>> test = oneOf [(==3), (==7), (==3), (==5)]
-- >>> test 1
-- False
--
-- >>> test 3
-- True
--
-- >>> test 5
-- True
oneOf :: [a -> Bool] -> a -> Bool
oneOf = foldr (\a b c -> a c || b c) (const False)

inSet :: (Eq a, Hashable a) => HashSet a -> a -> Bool
inSet = flip HashSet.member

hexDigit = inSet CharSet.hexDigit

{-
ident_start   [A-Za-z\200-\377_]
-}
firstIdentifierChar :: Char -> Bool
firstIdentifierChar x = isAlpha x || x == '_' || x >= '\200' && x <= '\377'

{-
ident_cont    [A-Za-z\200-\377_0-9\$]
-}
notFirstIdentifierChar :: Char -> Bool
notFirstIdentifierChar x = isAlphaNum x || x == '_' || x == '$' || x >= '\200' && x <= '\377'

keyword :: Text -> Bool
keyword = inSet KeywordSet.keyword

unreservedKeyword :: Text -> Bool
unreservedKeyword = inSet KeywordSet.unreservedKeyword

colNameKeyword :: Text -> Bool
colNameKeyword = inSet KeywordSet.colNameKeyword

typeFuncNameKeyword :: Text -> Bool
typeFuncNameKeyword = inSet KeywordSet.typeFuncNameKeyword

reservedKeyword :: Text -> Bool
reservedKeyword = inSet KeywordSet.reservedKeyword

{-# NOINLINE symbolicBinOpChar #-}
symbolicBinOpChar :: Char -> Bool
symbolicBinOpChar = inSet CharSet.symbolicBinOp

-- ** Op chars

opChar = inSet CharSet.op

prohibitedOpChar a = a == '+' || a == '-'

prohibitionLiftingOpChar = inSet CharSet.prohibitionLiftingOp
