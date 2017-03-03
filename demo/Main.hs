module Main where

import Prelude
import qualified PostgreSQL.Syntax as A


main =
  do
    demo ""
    demo "wrong"
    demo "SELECT *"
    demo "SELECT * FROM a"
    demo "SELECT"
    demo "SELECT from b"
    demo "SELECT * from b"
    demo "SELECT * from b WHERE"
    demo "SELECT * from b WHERE c"
    demo "SELECT * from b WHERE ?"
    demo "SELECT * from b WHERE $2"
  where
    demo =
      print . A.validate
