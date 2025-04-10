module Page.Security
  ( verifyToken,
    Result (..),

    -- * Typing rate limiting
    StrokeLimiter,
    newStrokeLimiter,
    observeStroke,
  )
where

import Control.Lens ((^.))
import Data.Aeson (Result (..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Int (Int64)
import Network.Wreq (FormParam ((:=)))
import qualified Network.Wreq as Wreq
import System.Clock (Clock (Monotonic), TimeSpec (sec), getTime)

-- | Server-side validation of Cloudflare Turnstile tokens.
-- See https://developers.cloudflare.com/turnstile/get-started/server-side-validation/.
verifyToken :: String -> String -> String -> String -> IO (Result ())
verifyToken secretKey token remoteIP idempotencyKey = do
  let url = "https://challenges.cloudflare.com/turnstile/v0/siteverify"
  let queryString =
        [ "secret" := secretKey,
          "response" := token,
          "remoteip" := remoteIP,
          "idempotency_key" := idempotencyKey
        ]
  r <- Wreq.post url queryString
  let body = r ^. Wreq.responseBody
  case Aeson.decode body of
    Just (Aeson.Object o) | KeyMap.member "success" o -> pure $ Success ()
    _ -> pure $ Error "Turnstile verification failed"

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
