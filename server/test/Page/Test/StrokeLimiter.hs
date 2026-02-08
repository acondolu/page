module Page.Test.StrokeLimiter (test) where

import Control.Monad (replicateM)
import Page.Security (StrokeLimiter, newStrokeLimiter, observeStroke)
import System.Clock (TimeSpec (..))
import Test.Tasty
import Test.Tasty.HUnit

at :: Int -> TimeSpec
at s = TimeSpec (fromIntegral s) 0

-- | Send n strokes all at the given time, return results.
strokes :: StrokeLimiter -> Int -> TimeSpec -> IO [Bool]
strokes lim n t = replicateM n (observeStroke lim t)

test :: TestTree
test =
  testGroup
    "Page.Security.StrokeLimiter"
    [ testCase "single stroke is allowed" $ do
        lim <- newStrokeLimiter
        r <- observeStroke lim (at 0)
        r @?= True,
      testCase "burst limit: 30 in 5s allowed, 31st rejected" $ do
        lim <- newStrokeLimiter
        rs <- strokes lim 30 (at 0)
        assertBool "first 30 allowed" (and rs)
        r <- observeStroke lim (at 0)
        r @?= False,
      testCase "burst limit resets after window" $ do
        lim <- newStrokeLimiter
        _ <- strokes lim 30 (at 0)
        -- 5 seconds later, burst window has moved on
        r <- observeStroke lim (at 5)
        r @?= True,
      testCase "sustained limit: 200 in 60s" $ do
        lim <- newStrokeLimiter
        -- 10 strokes every 3s: 20 slots × 10 = 200, gaps of 3 satisfy pauses,
        -- max 20 per 5s burst window
        rs <- concat <$> mapM (\s -> strokes lim 10 (at (s * 3))) [0 .. 19]
        assertBool "first 200 allowed" (and rs)
        -- 201st stroke still within the 60s window
        r <- observeStroke lim (at 58)
        r @?= False,
      testCase "sustained limit resets outside window" $ do
        lim <- newStrokeLimiter
        _ <- concat <$> mapM (\s -> strokes lim 10 (at (s * 3))) [0 .. 19]
        -- jump past the 60s window (latest entry at t=57)
        r <- observeStroke lim (at 117)
        r @?= True,
      testCase "pause requirement: no pauses rejected at 50+ strokes" $ do
        lim <- newStrokeLimiter
        -- 50 strokes all in second 0 — no pauses at all
        -- burst limit is 30 per 5s, so we need to spread across burst windows
        rs1 <- strokes lim 25 (at 0)
        rs2 <- strokes lim 25 (at 5)
        assertBool "first 50 allowed" (and rs1 && and rs2)
        -- next stroke: longCount >= 50 but pauseCount < 2
        r <- observeStroke lim (at 5)
        r @?= False,
      testCase "pause requirement: pauses satisfy check" $ do
        lim <- newStrokeLimiter
        -- spread strokes with gaps > 1s between groups
        -- 10 at t=0, 10 at t=3, 10 at t=6, 10 at t=9, 10 at t=12
        rs <- concat <$> mapM (\s -> strokes lim 10 (at (s * 3))) [0 .. 4]
        assertBool "50 with pauses allowed" (and rs)
        -- next stroke should be allowed since we have enough pauses
        r <- observeStroke lim (at 15)
        r @?= True,
      testCase "same-second strokes coalesce" $ do
        lim <- newStrokeLimiter
        _ <- observeStroke lim (at 0)
        _ <- observeStroke lim (at 0)
        r <- observeStroke lim (at 0)
        r @?= True
    ]
