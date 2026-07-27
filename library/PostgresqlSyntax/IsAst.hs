module PostgresqlSyntax.IsAst
  ( IsAst (..),
  )
where

import PostgresqlSyntax.Prelude

class IsAst a where
  toTextBuilder :: a -> TextBuilder
  parser :: Parser a
