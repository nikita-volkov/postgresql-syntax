module PostgresqlSyntax.Ast.TargetEl where

import HeadedMegaparsec
import PostgresqlSyntax.Ast.Ident
import PostgresqlSyntax.Ast.Internal
import {-# SOURCE #-} PostgresqlSyntax.Ast.AExpr (AExpr)
import PostgresqlSyntax.Extras.HeadedMegaparsec hiding (run)
import PostgresqlSyntax.IsAst
import qualified PostgresqlSyntax.KeywordSet as KeywordSet
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import Test.QuickCheck (scale)

-- |
-- ==== References
-- @
-- target_el:
--   |  a_expr AS ColLabel
--   |  a_expr IDENT
--   |  a_expr
--   |  '*'
-- @
data TargetEl
  = AliasedExprTargetEl AExpr Ident
  | ImplicitlyAliasedExprTargetEl AExpr Ident
  | ExprTargetEl AExpr
  | AsteriskTargetEl
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst TargetEl where
  toTextBuilder = \case
    AliasedExprTargetEl a b -> toTextBuilder a <> " AS " <> toTextBuilder b
    ImplicitlyAliasedExprTargetEl a b -> toTextBuilder a <> " " <> toTextBuilder b
    ExprTargetEl a -> toTextBuilder a
    AsteriskTargetEl -> "*"
  -- |
  -- >>> testParser targetEl "a.b as c"
  -- AliasedExprTargetEl (CExprAExpr (ColumnrefCExpr (Columnref (UnquotedIdent "a") (Just (AttrNameIndirectionEl (UnquotedIdent "b") :| []))))) (UnquotedIdent "c")
  parser =
    label "target"
      $ asum
        [ do
            expr <- parser
            asum
              [ do
                  space1
                  asum
                    [ AliasedExprTargetEl expr <$> (keyword "as" *> space1 *> endHead *> colLabel),
                      ImplicitlyAliasedExprTargetEl expr <$> parser
                    ],
                pure (ExprTargetEl expr)
              ],
          AsteriskTargetEl <$ char '*'
        ]
    where
      -- |
      -- Duplicated from "PostgresqlSyntax.Parsing"'s @colLabel@ (a
      -- bare-aliased 'PostgresqlSyntax.Ast.Ident' whose own, more
      -- permissive parser lives above this module in the dependency
      -- order), mirroring the 'PostgresqlSyntax.Ast.AnyName' precedent.
      colLabel =
        label "column label"
          $ keywordNameFromSet UnquotedIdent KeywordSet.keyword
          <|> parser

instance Arbitrary TargetEl where
  arbitrary =
    oneof
      [ pure AsteriskTargetEl,
        AliasedExprTargetEl <$> scale (`div` 2) arbitrary <*> arbitrary,
        ImplicitlyAliasedExprTargetEl <$> scale (`div` 2) arbitrary <*> arbitrary,
        ExprTargetEl <$> scale (`div` 2) arbitrary
      ]
