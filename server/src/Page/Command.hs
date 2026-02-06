module Page.Command
  ( SendTy (..),
    RecvTy (..),

    -- * For testing
    parseInteger,
    dumpInteger,
  )
where

import Data.Aeson
import Data.Aeson.Types (Parser, parseFail)
import Data.Text (Text)
import Numeric (readHex, showHex)

data SendTy
  = MoveRelative {x :: Int, y :: Int} -- ^ move by (x,y) relative
  | MoveAbsolute {ax :: Integer, ay :: Integer, rx :: Int, ry :: Int} -- ^ move to absolute (ax,ay), viewport-relative (rx,ry)
  | Resize {width :: Int, height :: Int} -- ^ resize viewport
  | WriteChar {x :: Int, y :: Int, c :: Char} -- ^ write character c at (x,y) relative
  | Ping {} -- ^ client ping: server sends deltas, then Pong
  | ReadRelative0 {} -- ^ send block at (0,0) relative
  | RenewToken {token :: String} -- ^ renew token for captcha verification
  deriving (Eq, Show)

instance FromJSON SendTy where
  parseJSON = withObject "SendTy" $ \o ->
    o .: "tag" >>= \case
      ("move-relative" :: Text) -> do
        x <- o .: "x"
        y <- o .: "y"
        pure MoveRelative {..}
      "move-absolute" -> do
        ax <- o .: "ax" >>= parseInteger
        ay <- o .: "ay" >>= parseInteger
        rx <- o .: "x"
        ry <- o .: "y"
        pure MoveAbsolute {..}
      "resize" -> do
        width <- o .: "width"
        height <- o .: "height"
        pure Resize {..}
      "write-char" -> do
        x <- o .: "x"
        y <- o .: "y"
        c <- o .: "c"
        pure WriteChar {..}
      "ping" -> pure Ping
      "read0" -> pure ReadRelative0
      "renew-token" -> do
        token <- o .: "token"
        pure RenewToken {..}
      tag -> parseFail $ "SendTy: unknown tag: " <> show tag

type Block = [Text] -- FIXME

data RecvTy
  = Rect {bx :: Int, by :: Int, text :: Block} -- ^ rectangle of text at (bx,by)
  | Done {} -- ^ all updates sent (unused)
  | Pong {} -- ^ server pong (all updates sent)

instance ToJSON RecvTy where
  toJSON Rect {..} =
    object
      [ "tag" .= ("rect" :: Text),
        "x" .= bx,
        "y" .= by,
        "text" .= text
      ]
  toJSON Done {} =
    object
      [ "tag" .= ("done" :: Text)
      ]
  toJSON Pong {} =
    object ["tag" .= ("pong" :: Text)]

---------------------------------------------------------------------
-- Integers (arbitrary-length ints) are represented as signed
-- hex strings (like say, in Python).

-- | Parse an Integer from an hex string.
parseInteger :: String -> Parser Integer
parseInteger "" = parseFail "parseInteger: empty string"
parseInteger ('-' : cs) = negate <$> parseInteger cs
parseInteger ('0' : 'x' : cs)
  | [(n, "")] <- readHex cs = pure n
parseInteger _ = parseFail "parseInteger: invalid hex string"

-- | Dump an Integer into an hex string.
dumpInteger :: Integer -> String
dumpInteger n
  | n < 0 = '-' : '0' : 'x' : showHex (negate n) ""
  | otherwise = '0' : 'x' : showHex n ""
