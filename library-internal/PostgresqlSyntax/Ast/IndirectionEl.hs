module PostgresqlSyntax.Ast.IndirectionEl where

import qualified HeadedMegaparsec as Parser
import {-# SOURCE #-} PostgresqlSyntax.Ast.AExpr (AExpr)
import PostgresqlSyntax.Ast.Ident
import qualified PostgresqlSyntax.Helpers.Gens as Gens
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
import PostgresqlSyntax.IsAst
import qualified PostgresqlSyntax.KeywordSet as KeywordSet
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import qualified Test.QuickCheck as Qc

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
  toTextBuilder settings = \case
    AttrNameIndirectionEl a -> "." <> toTextBuilder settings a
    AllIndirectionEl -> ".*"
    ExprIndirectionEl a -> TextBuilders.renderInBrackets (toTextBuilder settings a)
    SliceIndirectionEl a b -> TextBuilders.renderInBrackets (foldMap (toTextBuilder settings) a <> ":" <> foldMap (toTextBuilder settings) b)
  parser settings =
    asum
      [ do
          Parsers.char '.'
          Parser.endHead
          Parsers.space
          AllIndirectionEl <$ Parsers.char '*' <|> AttrNameIndirectionEl <$> colLabelLikeName,
        do
          Parsers.char '['
          Parser.endHead
          Parsers.space
          a <-
            asum
              [ do
                  Parsers.char ':'
                  Parser.endHead
                  Parsers.space
                  b <- optional (parser settings)
                  return (SliceIndirectionEl Nothing b),
                do
                  a <- parser settings
                  asum
                    [ do
                        Parsers.space
                        Parsers.char ':'
                        Parsers.space
                        b <- optional (parser settings)
                        return (SliceIndirectionEl (Just a) b),
                      return (ExprIndirectionEl a)
                    ]
              ]
          Parsers.space
          Parsers.char ']'
          return a
      ]
    where
      colLabelLikeName =
        Parser.label "column label" $
          Parsers.keywordNameFromSet UnquotedIdent KeywordSet.keyword
            <|> parser settings

instance Qc.Arbitrary IndirectionEl where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.sized $ \size ->
      if size <= 1
        then
          Qc.oneof
            [ AttrNameIndirectionEl <$> Qc.arbitrary,
              pure AllIndirectionEl,
              pure (SliceIndirectionEl Nothing Nothing)
            ]
        else
          Qc.oneof
            [ AttrNameIndirectionEl <$> Qc.arbitrary,
              pure AllIndirectionEl,
              ExprIndirectionEl <$> Gens.downscale Qc.arbitrary,
              SliceIndirectionEl <$> Gens.downscale Qc.arbitrary <*> Gens.downscale Qc.arbitrary
            ]
