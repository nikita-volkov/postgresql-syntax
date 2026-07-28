module PostgresqlSyntax.Ast.BExprIsOp where

import qualified HeadedMegaparsec as Parser
import {-# SOURCE #-} PostgresqlSyntax.Ast.BExpr (BExpr)
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Ast.TypeList
import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Parser
import qualified PostgresqlSyntax.Extras.QuickCheck as Qc
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import qualified Test.QuickCheck as Qc

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
      [ DistinctFromBExprIsOp <$> (keyphrase "distinct from" *> Parser.space1 *> Parser.endHead *> parser),
        OfBExprIsOp <$> (keyword "of" *> Parser.space1 *> Parser.endHead *> inParens parser),
        DocumentBExprIsOp <$ keyword "document"
      ]

instance Qc.Arbitrary BExprIsOp where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.oneofRec
      [ pure DocumentBExprIsOp
      ]
      [ DistinctFromBExprIsOp <$> Qc.arbitrary,
        OfBExprIsOp <$> Qc.arbitrary
      ]
