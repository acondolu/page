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
import Control.Lens ((^.), (&), (.~))
import Data.Aeson (Result (..), (.:))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (parseMaybe)
import Data.Int (Int64)
import Network.HTTP.Client (defaultManagerSettings, managerResponseTimeout, responseTimeoutMicro)
import Network.Wreq (FormParam ((:=)))
import qualified Network.Wreq as Wreq
import System.Clock (Clock (Monotonic), TimeSpec (sec), getTime)
import UnliftIO.Exception (tryAny)
import Data.Time

type Verify = String -> IO (Result UTCTime)

-- | Server-side validation of Cloudflare Turnstile tokens.
-- See https://developers.cloudflare.com/turnstile/get-started/server-side-validation/.
-- When successful, returns the token's expiration time (5 minutes after challenge timestamp).
verifyToken :: String -> String -> String -> String -> IO (Result UTCTime)
verifyToken secretKey token remoteIP idempotencyKey
  | length token > 2048 = pure $ Error "Token too long"
  | otherwise = retry (5 :: Int)
  where
    retry n = do
      mReq <- tryAny $ Wreq.postWith opts url queryString
      case mReq of
        Left _ -> maybeRetry n
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
                  | otherwise -> pure defaultError
                _ -> pure defaultError
            _ -> pure defaultError
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
      & Wreq.manager .~ Left (defaultManagerSettings { managerResponseTimeout = responseTimeoutMicro 10000 } )
    defaultError = Error "Turnstile verification failed"

parseIso :: String -> Maybe UTCTime
parseIso =
  parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ"

---------------------------------------------------------------------
-- Typing rate limiting.
-- Try and protect from flooding with a simple sliding window
-- approach.

type Sec = Int64

data StrokeLimiter = StrokeLimiter
  { -- | Start of current window
    timestamp :: Sec,
    -- | Number of keystrokes in the current window
    counter :: Int
  }
  deriving (Show)

newStrokeLimiter :: StrokeLimiter
newStrokeLimiter = StrokeLimiter 0 0

-- | Size of sliding window (in seconds).
windowSize :: Sec
windowSize = 5

-- | Maximum allowed keystrokes per window.
maxStrokes :: Int
maxStrokes = 30

-- | Ensure that the number of keystrokes within a sliding
-- time window does not exceed the allowed limit.
observeStroke :: StrokeLimiter -> IO (Maybe StrokeLimiter)
observeStroke (StrokeLimiter lastT lastN) = do
  now <- sec <$> getTime Monotonic
  pure $!
    if now - lastT <= windowSize
      then do
        -- period is still going
        let lastN' = lastN + 1
        if lastN' < maxStrokes
          then Just $ StrokeLimiter lastT lastN'
          else Nothing
      else do
        -- new period
        Just $ StrokeLimiter now 1
