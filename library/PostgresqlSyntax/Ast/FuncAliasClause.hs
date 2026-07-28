module PostgresqlSyntax.Ast.FuncAliasClause where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Ast.AliasClause
import PostgresqlSyntax.Ast.Ident
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Ast.TableFuncElementList
import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Parser
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import qualified Test.QuickCheck as Qc

-- |
-- ==== References
-- @
-- func_alias_clause:
--   | alias_clause
--   | AS '(' TableFuncElementList ')'
--   | AS ColId '(' TableFuncElementList ')'
--   | ColId '(' TableFuncElementList ')'
--   | EMPTY
-- @
data FuncAliasClause
  = AliasFuncAliasClause AliasClause
  | AsFuncAliasClause TableFuncElementList
  | AsColIdFuncAliasClause Ident TableFuncElementList
  | ColIdFuncAliasClause Ident TableFuncElementList
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst FuncAliasClause where
  toTextBuilder = \case
    AliasFuncAliasClause a -> toTextBuilder a
    AsFuncAliasClause a -> "AS (" <> toTextBuilder a <> ")"
    AsColIdFuncAliasClause a b -> "AS " <> toTextBuilder a <> " (" <> toTextBuilder b <> ")"
    ColIdFuncAliasClause a b -> toTextBuilder a <> " (" <> toTextBuilder b <> ")"
  parser =
    asum
      [ do
          _ <- keyword "as"
          asum
            [ do
                Parser.space
                inParens $ do
                  Parser.endHead
                  AsFuncAliasClause <$> parser,
              do
                Parser.space1
                a <- colId
                asum
                  [ do
                      Parser.space
                      inParens $ do
                        Parser.endHead
                        asum
                          [ AsColIdFuncAliasClause a <$> Parser.wrapToHead parser,
                            AliasFuncAliasClause . AliasClause True a . Just <$> parser
                          ],
                    pure (AliasFuncAliasClause (AliasClause True a Nothing))
                  ]
            ],
        do
          a <- colId
          asum
            [ do
                Parser.space
                inParens $ do
                  Parser.endHead
                  asum
                    [ ColIdFuncAliasClause a <$> Parser.wrapToHead parser,
                      AliasFuncAliasClause . AliasClause False a . Just <$> parser
                    ],
              pure (AliasFuncAliasClause (AliasClause False a Nothing))
            ]
      ]

instance Qc.Arbitrary FuncAliasClause where
  arbitrary =
    Qc.oneof
      [ AliasFuncAliasClause <$> Qc.arbitrary,
        AsFuncAliasClause <$> Qc.scale (`div` 2) Qc.arbitrary,
        AsColIdFuncAliasClause <$> Qc.arbitrary <*> Qc.scale (`div` 2) Qc.arbitrary,
        ColIdFuncAliasClause <$> Qc.arbitrary <*> Qc.scale (`div` 2) Qc.arbitrary
      ]
