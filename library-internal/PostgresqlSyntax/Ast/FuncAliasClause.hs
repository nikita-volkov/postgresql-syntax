module PostgresqlSyntax.Ast.FuncAliasClause where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Ast.AliasClause
import PostgresqlSyntax.Ast.Ident
import PostgresqlSyntax.Ast.TableFuncElementList
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude
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
  toTextBuilder settings = \case
    AliasFuncAliasClause a -> toTextBuilder settings a
    AsFuncAliasClause a -> "AS (" <> toTextBuilder settings a <> ")"
    AsColIdFuncAliasClause a b -> "AS " <> toTextBuilder settings a <> " (" <> toTextBuilder settings b <> ")"
    ColIdFuncAliasClause a b -> toTextBuilder settings a <> " (" <> toTextBuilder settings b <> ")"
  parser settings =
    asum
      [ do
          _ <- Parsers.keyword "as"
          asum
            [ do
                Parsers.space
                Parsers.inParens $ do
                  Parser.endHead
                  AsFuncAliasClause <$> parser settings,
              do
                Parsers.space1
                a <- colId settings
                asum
                  [ do
                      Parsers.space
                      Parsers.inParens $ do
                        Parser.endHead
                        asum
                          [ AsColIdFuncAliasClause a <$> Parser.wrapToHead (parser settings),
                            AliasFuncAliasClause . AliasClause True a . Just <$> parser settings
                          ],
                    pure (AliasFuncAliasClause (AliasClause True a Nothing))
                  ]
            ],
        do
          a <- colId settings
          asum
            [ do
                Parsers.space
                Parsers.inParens $ do
                  Parser.endHead
                  asum
                    [ ColIdFuncAliasClause a <$> Parser.wrapToHead (parser settings),
                      AliasFuncAliasClause . AliasClause False a . Just <$> parser settings
                    ],
              pure (AliasFuncAliasClause (AliasClause False a Nothing))
            ]
      ]

instance Qc.Arbitrary FuncAliasClause where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.oneof
      [ AliasFuncAliasClause <$> Qc.arbitrary,
        AsFuncAliasClause <$> Qc.arbitrary,
        AsColIdFuncAliasClause <$> Qc.arbitrary <*> Qc.arbitrary,
        ColIdFuncAliasClause <$> Qc.arbitrary <*> Qc.arbitrary
      ]
