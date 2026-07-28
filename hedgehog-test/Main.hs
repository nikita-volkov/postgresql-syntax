module Main where

import qualified Data.Text as Text
import Hedgehog
import Hedgehog.Main
import qualified Main.Gen as Gen
import PostgresqlSyntax (IsAst, parse, toText)
import Prelude

main :: IO ()
main =
  defaultMain
    [ checkParallel
        $ Group "Parsing a rendered AST produces the same AST"
        $ let p name amount gen =
                (,) name
                  $ withDiscards (fromIntegral amount * 200)
                  $ withTests amount
                  $ property
                  $ do
                    ast <- forAll gen
                    let sql = toText ast
                     in do
                          footnote ("SQL: " <> Text.unpack sql)
                          case parse sql of
                            Left err -> do
                              footnote err
                              failure
                            Right ast' -> ast === ast'
           in [ p "typename" 10000 Gen.typename,
                p "tableRef" 10000 Gen.tableRef,
                p "aExpr" 60000 Gen.aExpr,
                p "preparableStmt" 30000 Gen.preparableStmt
              ]
    ]
