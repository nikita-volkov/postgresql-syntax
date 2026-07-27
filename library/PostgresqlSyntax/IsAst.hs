module PostgresqlSyntax.IsAst
  ( IsAst (..),
    Parser,
  )
where

import HeadedMegaparsec (HeadedParsec)
import PostgresqlSyntax.Prelude
import TextBuilder (TextBuilder)

type Parser = HeadedParsec Void Text

class IsAst a where
  toTextBuilder :: a -> TextBuilder
  parser :: Parser a
