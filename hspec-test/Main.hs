module Main (main) where

import Prelude
import qualified Spec
import Test.Hspec

main :: IO ()
main = hspec $ parallel Spec.spec
