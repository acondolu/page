module Page.Config
  ( Config (..),
    CfTurnstileConfig (..),
    ServerConfig (..),
    BackupConfig (..),
    open,
  )
where

import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.Yaml as Yaml

-- | Configuration for Cloudflare Turnstile
data CfTurnstileConfig = CfTurnstileConfig
  { -- | Secret Key
    secretKey :: String,
    -- | Is this demo (dev) environment ?
    demo :: Bool
  }

instance FromJSON CfTurnstileConfig where
  parseJSON = withObject "CfTurnstileConfig" $ \o -> do
    secretKey <- o .: "secret-key"
    let demo = secretKey == "1x0000000000000000000000000000000AA"
    pure CfTurnstileConfig {..}

-- | Configuration for the WebSocket server
data ServerConfig = ServerConfig
  { port :: Int
  }

instance FromJSON ServerConfig where
  parseJSON = withObject "ServerConfig" $ \o -> do
    port <- o .: "port"
    pure ServerConfig {..}

-- | Configuration for database backup operations
data BackupConfig = BackupConfig
  { -- | Path to the final backup file where database state will be stored
    path :: FilePath,
    -- | Path to a temporary file used during backup process
    tmp :: FilePath
  }

instance FromJSON BackupConfig where
  parseJSON = withObject "BackupConfig" $ \o -> do
    path <- o .: "path"
    tmp <- o .: "tmp"
    pure BackupConfig {..}

-- | App configuration
data Config = Config
  { -- | Cloudflare Turnstile
    cfTurnstile :: CfTurnstileConfig,
    -- | WebSocket server
    server :: ServerConfig,
    -- | Database backup
    backup :: BackupConfig
  }

instance FromJSON Config where
  parseJSON = withObject "Config" $ \o -> do
    cfTurnstile <- o .: "cf-turnstile"
    server <- o .: "server"
    backup <- o .: "backup"
    pure Config {..}

parse :: ByteString -> Either String Config
parse bs = case Yaml.decodeEither' bs of
  Left err -> Left $ show err
  Right cfg -> Right cfg

-- | Parse the configuration from the given
-- file path.
open :: FilePath -> IO (Either String Config)
open fp =
  Yaml.decodeFileEither fp >>= \case
    Left err -> pure $ Left $ show err
    Right cfg -> pure $ Right cfg
