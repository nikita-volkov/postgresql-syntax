module PostgresqlSyntax.Ast.IndirectionEl where

import qualified HeadedMegaparsec as Parser
import {-# SOURCE #-} PostgresqlSyntax.Ast.AExpr (AExpr)
import PostgresqlSyntax.Ast.Ident
import PostgresqlSyntax.Ast.Internal
import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Parser
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
  toTextBuilder = \case
    AttrNameIndirectionEl a -> "." <> toTextBuilder a
    AllIndirectionEl -> ".*"
    ExprIndirectionEl a -> renderInBrackets (toTextBuilder a)
    SliceIndirectionEl a b -> renderInBrackets (foldMap toTextBuilder a <> ":" <> foldMap toTextBuilder b)
  parser =
    asum
      [ do
          Parser.char '.'
          Parser.endHead
          Parser.space
          AllIndirectionEl <$ Parser.char '*' <|> AttrNameIndirectionEl <$> colLabelLikeName,
        do
          Parser.char '['
          Parser.endHead
          Parser.space
          a <-
            asum
              [ do
                  Parser.char ':'
                  Parser.endHead
                  Parser.space
                  b <- optional parser
                  return (SliceIndirectionEl Nothing b),
                do
                  a <- parser
                  asum
                    [ do
                        Parser.space
                        Parser.char ':'
                        Parser.space
                        b <- optional parser
                        return (SliceIndirectionEl (Just a) b),
                      return (ExprIndirectionEl a)
                    ]
              ]
          Parser.space
          Parser.char ']'
          return a
      ]
    where
      colLabelLikeName =
        Parser.label "column label"
          $ keywordNameFromSet UnquotedIdent KeywordSet.keyword
          <|> parser

instance Qc.Arbitrary IndirectionEl where
  arbitrary =
    Qc.oneof
      [ AttrNameIndirectionEl <$> Qc.arbitrary,
        pure AllIndirectionEl,
        ExprIndirectionEl <$> Qc.scale (`div` 2) Qc.arbitrary,
        SliceIndirectionEl <$> Qc.scale (`div` 2) Qc.arbitrary <*> Qc.scale (`div` 2) Qc.arbitrary
      ]
