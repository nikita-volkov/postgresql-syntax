{-# OPTIONS_GHC -Wno-missing-signatures #-}

-- |
-- Repeatable timing harness for the nesting-depth blowup described in
-- @docs/tasks/parser-exponential-time-on-nesting.md@.
--
-- Run with @cabal run nesting-bench@. Prints a table of parse times so the
-- before\/after effect of a fix is measurable rather than asserted.
--
-- Each case is given a wall-clock budget; a case that exceeds it is reported
-- as a timeout instead of hanging the run.
module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified PostgresqlSyntax.Parsing as Parsing
import System.Clock
import Prelude

-- * Inputs

-- | Case 1: @((((a + b))))@ with @n@ layers of redundant parentheses.
nestedParens :: Int -> Text
nestedParens n =
  Text.replicate n "(" <> "a + b" <> Text.replicate n ")"

-- | Case 2: the user-reported shape — a sum of @COALESCE@ terms, split into
-- two parenthesised groups subtracted from one another, wrapped in a few
-- redundant parentheses.
coalesceSum :: Int -> Int -> Text
coalesceSum terms wrap =
  Text.replicate wrap "("
    <> group 0
    <> " - "
    <> group terms
    <> Text.replicate wrap ")"
  where
    group off =
      "(" <> Text.intercalate " + " [term (off + i) | i <- [1 .. terms]] <> ")"
    term i =
      "coalesce(c" <> Text.pack (show i) <> ", 0)"

-- * Timing

-- | Run an action under a wall-clock budget. 'Nothing' on timeout.
withTimeout :: Int -> IO a -> IO (Maybe a)
withTimeout micros action = do
  slot <- newEmptyMVar
  _ <- forkIO (action >>= \x -> putMVar slot (Just x))
  _ <- forkIO (threadDelay micros >> putMVar slot Nothing)
  takeMVar slot

-- | Parse and fully force the result, returning elapsed seconds.
time :: Text -> IO Double
time input = do
  start <- getTime Monotonic
  _ <- evaluate (force (either (const "err") (const "ok") (Parsing.run Parsing.aExpr input) :: String))
  end <- getTime Monotonic
  pure (fromIntegral (toNanoSecs (diffTimeSpec end start)) / 1e9)

budgetSeconds :: Double
budgetSeconds = 10

measure :: String -> Text -> IO ()
measure label input = do
  result <- withTimeout (round (budgetSeconds * 1e6)) (time input)
  putStrLn $ case result of
    Nothing -> pad label <> "  TIMEOUT (>" <> show (round budgetSeconds :: Int) <> "s), " <> show (Text.length input) <> " chars"
    Just seconds -> pad label <> "  " <> show seconds <> "s, " <> show (Text.length input) <> " chars"
  where
    pad s = s <> replicate (max 0 (28 - length s)) ' '

main :: IO ()
main = do
  putStrLn "Case 1: ((((a + b)))) at increasing nesting depth"
  mapM_ (\n -> measure ("  depth " <> show n) (nestedParens n)) [1, 2, 4, 6, 8, 10, 12, 14, 16, 20, 30, 50, 100, 500]
  putStrLn ""
  putStrLn "Case 2: sum-of-COALESCE, two groups, 6 redundant wraps"
  mapM_ (\t -> measure ("  " <> show (2 * t) <> " terms") (coalesceSum t 6)) [4, 8, 12, 16, 20, 24]
