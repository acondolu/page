module Page.Security
  ( Verify,
    verifyToken,
    Result (..),

    -- * Typing rate limiting
    StrokeLimiter,
    newStrokeLimiter,
    observeStroke,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically)
import Control.Lens ((^.), (&), (.~))
import Data.Aeson (Result (..), (.:))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (parseMaybe)
import Data.Int (Int64)
import Network.HTTP.Client (managerResponseTimeout, responseTimeoutMicro)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Wreq (FormParam ((:=)))
import qualified Network.Wreq as Wreq
import System.Clock (TimeSpec (sec))
import UnliftIO.Exception (tryAny)
import Data.Time
import qualified Page.RingBuffer as RingBuffer
import Prelude hiding (log)

type Verify = String -> IO (Result UTCTime)

type Logger = String -> IO ()

-- | Server-side validation of Cloudflare Turnstile tokens.
-- See https://developers.cloudflare.com/turnstile/get-started/server-side-validation/.
-- When successful, returns the token's expiration time (5 minutes after challenge timestamp).
verifyToken :: Logger -> String -> String -> String -> String -> IO (Result UTCTime)
verifyToken log secretKey token remoteIP idempotencyKey
  | length token > 2048 = pure $ Error "Token too long"
  | otherwise = retry (5 :: Int)
  where
    retry n = do
      mReq <- tryAny $ Wreq.postWith opts url queryString
      case mReq of
        Left err -> do
          log $ "turnstile: error: " ++ show err
          maybeRetry n
        Right r -> do
          let body = r ^. Wreq.responseBody
          case Aeson.decode body of
            Just (Aeson.Object o) -> do
              let success = parseMaybe (.: "success") o
                  errors = parseMaybe (.: "error-codes") o
                  mChallengeTs = parseMaybe (.: "challenge_ts") o >>= parseIso
              case (success, errors :: Maybe [String], mChallengeTs) of
                (Just True, _, Just challengeTs) ->
                  -- expire in 5 minutes
                  pure $ Success (300 `addUTCTime` challengeTs)
                (_, Just errs, _)
                  | "internal-error" `elem` errs -> maybeRetry n
                  | otherwise -> do
                      log $ "turnstile: failed: " ++ show body
                      pure defaultError
                _ -> do
                  log $ "turnstile: failed: " ++ show body
                  pure defaultError
            _ -> do
              log $ "turnstile: failed: " ++ show body
              pure defaultError
    maybeRetry n
      | n > 0 = do
          threadDelay (2 ^ (5 - n) * 1000000)
          retry (n - 1)
      | otherwise = pure defaultError
    url = "https://challenges.cloudflare.com/turnstile/v0/siteverify"
    queryString =
      [ "secret" := secretKey,
        "response" := token,
        "remoteip" := remoteIP,
        "idempotency_key" := idempotencyKey
      ]
    opts = Wreq.defaults
      & Wreq.manager .~ Left (tlsManagerSettings { managerResponseTimeout = responseTimeoutMicro 10000000 }) -- 10s
    defaultError = Error "turnstile: verification failed"

parseIso :: String -> Maybe UTCTime
parseIso =
  parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ"

---------------------------------------------------------------------
-- Typing rate limiting.
-- Sliding window approach with multiple signals:
--   1. Burst protection: max 30 strokes per 5 seconds
--   2. Sustained rate: max 200 strokes per minute
--   3. Natural pauses: require some >1s gaps in the last minute

type Sec = Int64

-- | Sliding window rate limiter storing per-second stroke counts.
newtype StrokeLimiter = StrokeLimiter (RingBuffer.RingBuffer (Sec, Int))

-- Short window (burst protection)
shortWindow, longWindow :: Sec
shortWindow = 5
longWindow = 60

maxShortStrokes, maxLongStrokes, minPauses :: Int
maxShortStrokes = 30
maxLongStrokes = 200
minPauses = 2

newStrokeLimiter :: IO StrokeLimiter
newStrokeLimiter = StrokeLimiter <$> RingBuffer.new (fromIntegral longWindow)

-- | Observe a keystroke. Returns True if allowed, False if rate limited.
observeStroke :: StrokeLimiter -> TimeSpec -> IO Bool
observeStroke (StrokeLimiter buf) ts = atomically $ do
  let now = sec ts
  entries <- RingBuffer.toList buf
  let shortCount = sum [n | (t, n) <- entries, t > now - shortWindow]
      longEntries = [(t, n) | (t, n) <- entries, t > now - longWindow]
      longCount = sum $ map snd longEntries
      pauseCount = countPauses $ map fst longEntries
      allowed = shortCount < maxShortStrokes
             && longCount < maxLongStrokes
             && (longCount < 50 || pauseCount >= minPauses)
  if allowed
    then do
      case entries of
        _ | Just (t, n) <- lastMay entries, t == now ->
            RingBuffer.replaceLast buf (now, n + 1)
        _ -> RingBuffer.push buf (now, 1)
      pure True
    else pure False

-- | Count gaps > 1 second between consecutive timestamps.
countPauses :: [Sec] -> Int
countPauses ts = length $ filter (> 1) $ zipWith (-) (drop 1 ts) ts

lastMay :: [a] -> Maybe a
lastMay [] = Nothing
lastMay xs = Just $ last xs
