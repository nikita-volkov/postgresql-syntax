module PostgresqlSyntax.Ast.IndirectionEl where

import HeadedMegaparsec
import {-# SOURCE #-} PostgresqlSyntax.Ast.AExpr (AExpr)
import PostgresqlSyntax.Ast.Ident
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Extras.HeadedMegaparsec hiding (run)
import PostgresqlSyntax.IsAst
import qualified PostgresqlSyntax.KeywordSet as KeywordSet
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import Test.QuickCheck (scale)

-- |
-- 'PostgresqlSyntax.Ast.AttrName' is a bare alias to 'PostgresqlSyntax.Ast.ColLabel',
-- which is a bare alias to 'Ident', but the @ColLabel@ /parser/ (kept in
-- "PostgresqlSyntax.Parsing" since @ColLabel@ itself isn't extracted in this
-- batch) is more permissive than plain 'Ident'. Since this module sits below
-- "PostgresqlSyntax.Parsing" (no import cycle allowed), that ColLabel-flavored
-- element parser is duplicated here, same as 'PostgresqlSyntax.Ast.NameList'.
--
-- ==== References
-- @
-- indirection_el:
--   |  '.' attr_name
--   |  '.' '*'
--   |  '[' a_expr ']'
--   |  '[' opt_slice_bound ':' opt_slice_bound ']'
-- opt_slice_bound:
--   |  a_expr
--   |  EMPTY
-- @
data IndirectionEl
  = AttrNameIndirectionEl Ident
  | AllIndirectionEl
  | ExprIndirectionEl AExpr
  | SliceIndirectionEl (Maybe AExpr) (Maybe AExpr)
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst IndirectionEl where
  toTextBuilder = \case
    AttrNameIndirectionEl a -> "." <> toTextBuilder a
    AllIndirectionEl -> ".*"
    ExprIndirectionEl a -> renderInBrackets (toTextBuilder a)
    SliceIndirectionEl a b -> renderInBrackets (foldMap toTextBuilder a <> ":" <> foldMap toTextBuilder b)
  parser =
    asum
      [ do
          char '.'
          endHead
          space
          AllIndirectionEl <$ char '*' <|> AttrNameIndirectionEl <$> colLabelLikeName,
        do
          char '['
          endHead
          space
          a <-
            asum
              [ do
                  char ':'
                  endHead
                  space
                  b <- optional parser
                  return (SliceIndirectionEl Nothing b),
                do
                  a <- parser
                  asum
                    [ do
                        space
                        char ':'
                        space
                        b <- optional parser
                        return (SliceIndirectionEl (Just a) b),
                      return (ExprIndirectionEl a)
                    ]
              ]
          space
          char ']'
          return a
      ]
    where
      colLabelLikeName =
        label "column label"
          $ keywordNameFromSet UnquotedIdent KeywordSet.keyword
          <|> parser

instance Arbitrary IndirectionEl where
  arbitrary =
    oneof
      [ AttrNameIndirectionEl <$> arbitrary,
        pure AllIndirectionEl,
        ExprIndirectionEl <$> scale (`div` 2) arbitrary,
        SliceIndirectionEl <$> scale (`div` 2) arbitrary <*> scale (`div` 2) arbitrary
      ]
