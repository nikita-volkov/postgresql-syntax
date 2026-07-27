module PostgresqlSyntax.Ast.BExprIsOp where

import HeadedMegaparsec
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Ast.TypeList
import {-# SOURCE #-} PostgresqlSyntax.Ast.BExpr (BExpr)
import PostgresqlSyntax.Extras.HeadedMegaparsec hiding (run)
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import Test.QuickCheck (scale)

-- |
-- Renders\/parses only the \"positive\" form (@DISTINCT FROM ...@\/@OF
-- (...)@\/@DOCUMENT@), mirroring 'PostgresqlSyntax.Ast.AExprReversableOp':
-- the external @IS@\/@IS NOT@ toggle lives alongside this type in
-- 'PostgresqlSyntax.Ast.BExpr'\'s own @IsOpBExpr BExpr Bool BExprIsOp@
-- constructor and is rendered\/parsed there, not here.
--
-- ==== References
-- @
--   | b_expr IS DISTINCT FROM b_expr
--   | b_expr IS NOT DISTINCT FROM b_expr
--   | b_expr IS OF '(' type_list ')'
--   | b_expr IS NOT OF '(' type_list ')'
--   | b_expr IS DOCUMENT_P
--   | b_expr IS NOT DOCUMENT_P
-- @
data BExprIsOp
  = DistinctFromBExprIsOp BExpr
  | OfBExprIsOp TypeList
  | DocumentBExprIsOp
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst BExprIsOp where
  toTextBuilder = \case
    DistinctFromBExprIsOp b -> "DISTINCT FROM " <> toTextBuilder b
    OfBExprIsOp b -> "OF " <> renderInParens (toTextBuilder b)
    DocumentBExprIsOp -> "DOCUMENT"
  parser =
    asum
      [ DistinctFromBExprIsOp <$> (keyphrase "distinct from" *> space1 *> endHead *> parser),
        OfBExprIsOp <$> (keyword "of" *> space1 *> endHead *> inParens parser),
        DocumentBExprIsOp <$ keyword "document"
      ]

instance Arbitrary BExprIsOp where
  arbitrary =
    oneof
      [ DistinctFromBExprIsOp <$> scale (`div` 2) arbitrary,
        OfBExprIsOp <$> scale (`div` 2) arbitrary,
        pure DocumentBExprIsOp
      ]
