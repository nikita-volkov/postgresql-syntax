module PostgresqlSyntax.Extras.TextBuilder where

import PostgresqlSyntax.Prelude
import TextBuilder

char7 :: Char -> TextBuilder
char7 = char

intDec :: Int -> TextBuilder
intDec = decimal

int64Dec :: Int64 -> TextBuilder
int64Dec = decimal

doubleDec :: Double -> TextBuilder
doubleDec = fromString . show
