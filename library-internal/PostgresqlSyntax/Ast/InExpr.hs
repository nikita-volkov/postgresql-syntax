module PostgresqlSyntax.Ast.InExpr
  ( InExpr (..),
  )
where

import qualified HeadedMegaparsec as Parser
import {-# SOURCE #-} PostgresqlSyntax.Ast.AExpr (selectWithParensAExpr)
import PostgresqlSyntax.Ast.ExprList
import {-# SOURCE #-} PostgresqlSyntax.Ast.SelectWithParens (SelectWithParens, withParensSelectWithParensInner)
import qualified PostgresqlSyntax.Helpers.Gens as Gens
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import qualified Test.QuickCheck as Qc
import qualified Text.Megaparsec as Megaparsec

-- |
-- ==== References
-- @
-- in_expr:
--   | select_with_parens
--   | '(' expr_list ')'
-- @
data InExpr
  = SelectInExpr SelectWithParens
  | ExprListInExpr ExprList
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst InExpr where
  toTextBuilder = \case
    SelectInExpr a -> toTextBuilder a
    ExprListInExpr a -> TextBuilders.renderInParens (toTextBuilder a)
  parser =
    (ExprListInExpr <$> Parser.parse (Megaparsec.try (Parser.toParsec (Parsers.inParens parser))))
      <|> (SelectInExpr <$> Parser.wrapToHead parser)

instance Qc.Arbitrary InExpr where
  shrink = fmap canonicalize . Qc.genericShrink
  arbitrary =
    canonicalize
      <$> Qc.sized
        ( \n ->
            if n <= 1
              then ExprListInExpr <$> Qc.arbitrary
              else
                Qc.oneof
                  [ SelectInExpr <$> Gens.downscale Qc.arbitrary,
                    ExprListInExpr <$> Qc.arbitrary
                  ]
        )

-- |
-- Collapses the non-canonical @SelectInExpr@-of-@WithParensSelectWithParens@
-- shape to the @ExprListInExpr@ shape the parser actually produces for it:
-- @in_expr@'s two productions, @select_with_parens@ and
-- @'(' expr_list ')'@, overlap whenever the parenthesised select itself
-- contains another parenthesised select, since a single-element
-- @expr_list@ can itself be that inner parenthesised select wrapped as a
-- @c_expr@. Both render to the same text; the parser's @expr_list@
-- alternative is tried first and so wins. Both 'arbitrary' and 'shrink'
-- can otherwise construct the non-canonical shape, which renders fine but
-- parses back to a different, canonical value and so breaks the
-- roundtrip property.
canonicalize :: InExpr -> InExpr
canonicalize = \case
  SelectInExpr a
    | Just inner <- withParensSelectWithParensInner a ->
        ExprListInExpr (ExprList (selectWithParensAExpr inner :| []))
  other -> other
