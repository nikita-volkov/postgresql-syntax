module PostgresqlSyntax.Extras.TextBuilder where

import PostgresqlSyntax.Prelude
import Text.Builder


char7 :: Char -> Builder
char7 = char

intDec :: Int -> Builder
intDec = decimal

int64Dec :: Int64 -> Builder
int64Dec = decimal

doubleDec :: Double -> Builder
doubleDec = fromString . show
