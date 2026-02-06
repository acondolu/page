{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}

module Main (main, main') where

import Control.Monad (forM_, unless, when)
import Data.Aeson (eitherDecode, encode)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.ByteString.Lazy (ByteString)
import Data.List.Split (splitOn)
import Data.Time (UTCTime, getCurrentTime, addUTCTime)
import Data.Word (Word32, Word64)
import qualified Network.WebSockets as WS
import qualified Options.Applicative as OptParse
import qualified Page.Command as Command
import qualified Page.Config as Config
import qualified Page.TUI as TUI
import Page.Constants
import qualified Page.Database as Database
import qualified Page.Database.Cursor as Cursor
import qualified Page.Geometry as Geometry
import Page.Geometry.Coordinates
import qualified Page.Security as Security
import System.Exit (exitFailure)
import System.Log.FastLogger (newStderrLoggerSet, pushLogStrLn, rmLoggerSet, toLogStr)
import UnliftIO (bracket, catchAny, readIORef, throwIO, withAsync)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Directory (renameFile)
import Prelude hiding (log)

main :: IO ()
main = OptParse.execParser opts >>= main'
  where
    opts = OptParse.info (parser OptParse.<**> OptParse.helper) OptParse.fullDesc
    parser =
      OptParse.argument OptParse.str $
        OptParse.metavar "PATH"
          <> OptParse.help "Path to configuration file"

-- | Like 'main', but the path to the configuration
-- file is passed as an argument.
main' :: FilePath -> IO ()
main' fp =
  withLogger $ \log -> do
    Config.open fp >>= \case
      Left err -> do
        log $ "main': config file error: " ++ show err
        exitFailure
      Right cfg -> do
        let port = Config.port $ Config.server cfg
            backupCfg = Config.backup cfg
            robotMode = Config.robots cfg
        db <- load log backupCfg robotMode
        let telnet = case Config.telnetPort cfg of
              Just p -> TUI.startTUI log db p
              Nothing -> pure ()
        withAsync telnet $ \_ ->
          withAsync (backupDaemon log backupCfg db) $ \_ ->
            WS.runServer "0.0.0.0" port $ application log cfg db
  where
    withLogger act = bracket (newStderrLoggerSet 0) rmLoggerSet $
      \loggerSet -> act (pushLogStrLn loggerSet . toLogStr)

type Logger = String -> IO ()

-- | Security checks on incoming connection
verify :: Logger -> Config.CfTurnstileConfig -> WS.PendingConnection -> IO (Security.Result (UTCTime, Security.Verify))
verify log cfg pending = withLogErrors $ do
  let headers = WS.requestHeaders $ WS.pendingRequest pending
  let secretKey = Config.secretKey cfg
  let remoteIP'
        | Config.demo cfg = Just "127.0.0.1"
        | otherwise = BS8.unpack <$> lookup "CF-Connecting-IP" headers
  case remoteIP' of
    Nothing -> pure $ Security.Error "CF-Connecting-IP missing"
    Just remoteIP ->
      case lookup "Sec-WebSocket-Protocol" headers of
        Just subprotos ->
          case splitOn ", " $ BS8.unpack subprotos of
            ["page.acondolu.me", token, idempotencyKey] -> do
              let verifyFun = \tok -> Security.verifyToken secretKey tok remoteIP idempotencyKey
              verifyFun token >>= \case
                Security.Success verifyExpireTs -> pure $ Security.Success (verifyExpireTs, verifyFun)
                Security.Error err -> pure $ Security.Error err
            _ -> pure $ Security.Error "Invalid Sec-WebSocket-Protocol"
        _ -> pure $ Security.Error "Sec-WebSocket-Protocol missing"
  where
    withLogErrors act = catchAny act $ \e -> do
      log ("verify: " <> show e)
      throwIO e

application :: Logger -> Config.Config -> Database.DB -> WS.ServerApp
application log config db pending = do
  let robotMode = Config.robots config
  result <- if robotMode
    then do
      verifyExpireTs <- addUTCTime (24 * 3600) <$> getCurrentTime
      pure $ Security.Success (verifyExpireTs, \_ -> pure $ Security.Success verifyExpireTs)
    else verify log (Config.cfTurnstile config) pending
  case result of
    Security.Error str -> do
      WS.rejectRequest pending $ BS8.pack str
    Security.Success (verifyExpireTs, reVerify) -> do
      conn <- WS.acceptRequestWith
            pending
            WS.defaultAcceptRequest
              { WS.acceptHeaders =
                  [ ("Sec-WebSocket-Protocol", "page.acondolu.me")
                  ]
              }
      handleConnection log verifyExpireTs reVerify robotMode db conn `catchAny` \e ->
        log $ "handleConnection: " <> show e

welcomeMessage :: [String]
welcomeMessage =
  [ "+------------------------------------------+ ",
    "| This is <page>, an infinite document and | ",
    "| collaborative project.                   | ",
    "| You can type anywhere: the page extends  | ",
    "| indefinitely in all directions.          | ",
    "+------------------------------------------+ ",
    "| You can simply scroll the page, or jump  | ",
    "| to any coordinate by clicking on the     | ",
    "| coordinate box below.                    | ",
    "+------------------------------------------+ ",
    "| /!\\ Be creative and civil.               | ",
    "| By participating, you agree not to post  | ",
    "| illegal or harmful content.              | ",
    "| Thank you! Have fun :-) <3               | ",
    "+------------------------------------------+ ",
    "| =>  http://github.com/acondolu/page   <= | ",
    "+------------------------------------------+ ",
    "                                             "
  ]

-------------------------------------------------------------------------------
-- Connection

-- | The state of a connection.
data ConnState = ConnState
  { -- | Absolute x-coordinate
    ax :: Integer,
    -- | Absolute y-coordinate
    ay :: Integer,
    -- | Relative x-component
    dx :: Word32,
    -- | Relative y-component
    dy :: Word32,
    -- | Relative x-component
    rx :: Int,
    -- | Relative y-component
    ry :: Int,
    -- | Viewport width
    w :: Int,
    -- | Viewport height
    h :: Int,
    -- | Indicates whether the state is near zero.
    -- Updated on absolute moves.
    -- Used by 'isForbidden'.
    isNearZero :: Bool,
    -- | Database cursor.
    cursor :: Cursor.Cursor,
    -- | Last revision number. Used for TODO
    lastRevision :: Word64,
    -- | Stroke limiter for security purposes.
    strokes :: Security.StrokeLimiter,
    -- | Expiration time for Turnstile token.
    verifyExpires :: UTCTime
  }

viewport :: ConnState -> Geometry.Rect Int
viewport state
  | emptyViewport state = Geometry.Rect 0 0 0 0
viewport ConnState {dx, dy, rx, ry, w, h} = do
  let x1 = fromIntegral dx + rx
      y1 = fromIntegral dy + ry
      x2 = x1 + w
      y2 = y1 + h
  let x1' = x1 `divFloor` block_size
      y1' = y1 `divFloor` block_size
      x2' = x2 `divCeil` block_size
      y2' = y2 `divCeil` block_size
  -- expand by 1 block in all directions to make
  -- scrolling more seemless
  Geometry.Rect (x1' - 1) (y1' - 1) (x2' + 1) (y2' + 1)
  where
    divFloor :: Int -> Int -> Int
    divFloor a b = div a b
    divCeil :: Int -> Int -> Int
    divCeil a b = -(div (-a) b)

emptyViewport :: ConnState -> Bool
emptyViewport ConnState {w, h} = w <= 0 || h <= 0

handleConnection :: Logger -> UTCTime -> Security.Verify -> Bool -> Database.DB -> WS.Connection -> IO ()
handleConnection log verifyExpireTs reVerify robotMode db conn = do
  lastRevision <- Database.revision db
  cursor <- Cursor.origin db
  loop $
    ConnState
      { ax = 0,
        ay = 0,
        dx = 0,
        dy = 0,
        rx = 0,
        ry = 0,
        w = 0,
        h = 0,
        isNearZero = True,
        cursor,
        lastRevision,
        strokes = Security.newStrokeLimiter,
        verifyExpires = verifyExpireTs
      }
  where
    loop !state = do
      cmd <- recv conn
      -- check if session expired
      now <- getCurrentTime
      when (verifyExpires state < now) $
        abort conn "Turnstile token expired"
      unless (cmd == Command.Ping) $
        log $ "handleConnection: incoming: " <> show cmd
      case cmd of
        Command.MoveRelative {x, y} -> do
          let state' = state {rx = x, ry = y}
          sendDiff state state'
          loop state'
        Command.Resize {width, height} -> do
          unless (resizeOkay width height) $
            abort conn "viewport size must be positive"
          let state' = state {w = width, h = height}
          sendDiff state state'
          loop state'
        Command.WriteChar {x, y, c} -> do
          mStrokes <- if robotMode
            then pure $ Just $ strokes state
            else Security.observeStroke (strokes state)
          case mStrokes of
            Nothing -> abort conn "flood"
            Just strokes' -> do
              let x' = fromIntegral (dx state) + x
                  y' = fromIntegral (dy state) + y
                  coord = TileRelativeCharCoord' x' y'
              unless (not robotMode && isForbidden state x y) $
                Cursor.writeCharAt (cursor state) coord c
              loop state {strokes = strokes'}
        Command.MoveAbsolute {ax, ay, rx, ry} -> do
          unless (moveAbsoluteOkay ax ay rx ry) $
            abort conn "illegal absolute move"
          let isNearZero =
                abs ax <= 2 * fromIntegral (maxBound :: Int)
                  && abs ay < 2 * fromIntegral (maxBound :: Int)
          let ac = AbsoluteCharCoord ax ay
              (tileCoord, charCoord) = absoluteCharCoord ac
              AbsoluteTileCoord ax' ay' = tileCoord
              TileRelativeCharCoord dx dy = charCoord
          cursor' <- Cursor.jumpTo tileCoord (cursor state)
          let ax'' = ax' * (block_size * quad_size)
              ay'' = ay' * (block_size * quad_size)
          let state' =
                state
                  { ax = ax'',
                    ay = ay'',
                    dx,
                    dy,
                    rx,
                    ry,
                    isNearZero,
                    cursor = cursor'
                  }
          sendDiff state {w = 0, h = 0} state'
          loop state'
        Command.Ping -> do
          rev <- Cursor.cRevision (cursor state)
          sendDeltas state
          WS.sendTextData conn $ encode Command.Pong
          loop state {lastRevision = rev}
        Command.ReadRelative0 {} -> do
          let Geometry.Rect x1 y1 _ _ = viewport state
          let area = Geometry.Area [Geometry.Rect x1 y1 x1 y1]
          regions <- Cursor.query (cursor state) area
          case regions of
            [] -> send conn $ Command.Rect 0 0 $ replicate block_size "                "
            _ ->
              forM_ regions $ \region ->
                send conn $ blockToRect state region
          loop state
        Command.RenewToken {token} -> do
          reVerify token >>= \case
            Security.Success verifyExpireTs' ->
              loop state {verifyExpires = verifyExpireTs'}
            Security.Error err -> abort conn $ BL8.pack err

    -- send new blocks visible due to change of viewport
    sendDiff _state state' | emptyViewport state' = do
      WS.sendTextData conn $ encode Command.Done
    sendDiff state state' = do
      let area = Geometry.rdiff (viewport state') (viewport state)
      unless (Geometry.nullA area) $ do
        regions <- Cursor.query (cursor state') area
        forM_ regions $ \region ->
          send conn $ blockToRect state' region
      WS.sendTextData conn $ encode Command.Done

    -- send blocks that have changed since last revision
    sendDeltas state = do
      let area = Geometry.Area [viewport state]
      regions <- Cursor.query (cursor state) area
      forM_ regions $ \region@(Geometry.Pinned _ _ (Database.Block modif _)) -> do
        m <- readIORef modif
        let doSend = m > lastRevision state
        when doSend $ do
          log "pong"
          send conn $ blockToRect state region
      WS.sendTextData conn $ encode Command.Done

abort :: WS.Connection -> ByteString -> IO ()
abort conn bs = do
  WS.sendClose conn bs
  ioError $ userError $ "abort: " <> show bs

-- | Check if the character is in forbidden area
-- (the welcome message area). It is not allowed
-- to overwrite the welcome message.
isForbidden :: ConnState -> Int -> Int -> Bool
isForbidden ConnState {isNearZero, ax, ay, dx, dy} x y
  | isNearZero = do
      let w = fromIntegral $ length $ head welcomeMessage
          h = fromIntegral $ length welcomeMessage
          x' = ax + fromIntegral dx + fromIntegral x
          y' = ay + fromIntegral dy + fromIntegral y
      0 <= x' && x' < w && 0 <= y' && y' < h
  | otherwise = False

-- | Enforce some invariants on coordinates.
moveAbsoluteOkay :: Integer -> Integer -> Int -> Int -> Bool
moveAbsoluteOkay ax ay rx ry =
  ax `mod` block_size == 0
    && ay `mod` block_size == 0
    && 0 <= rx
    && rx < block_size
    && 0 <= ry
    && ry < block_size

-- | Check if resize is valid. Maximum allowed
-- screen size is 16384 x 16384 .
resizeOkay :: Int -> Int -> Bool
resizeOkay width height =
  width >= 0
    && width < 16384
    && height >= 0
    && height < 16384

blockToRect :: ConnState -> Geometry.Pinned Int Database.Block -> Command.RecvTy
blockToRect ConnState {dx, dy} (Geometry.Pinned x y bs) =
  Command.Rect (x * block_size - fromIntegral dx) (y * block_size - fromIntegral dy) $ Database.blockToTexts bs

-- | Send message to client.
send :: WS.Connection -> Command.RecvTy -> IO ()
send conn x = WS.sendTextData conn $ encode x

-- | Recv message from client.
recv :: WS.Connection -> IO Command.SendTy
recv conn = do
  msg <- WS.receiveDataMessage conn
  case msg of
    WS.Binary bs -> ioError $ userError $ "recv: Binary: " <> show bs
    WS.Text bs _ -> case eitherDecode bs of
      Left err -> ioError $ userError $ "recv: invalid message: " <> show err
      Right cmd -> pure cmd

-------------------------------------------------------------------------------
-- Load / Save Database

load :: Logger -> Config.BackupConfig -> Bool -> IO Database.DB
load log Config.BackupConfig {path} robotMode = do
  db <-
    Database.load path `catchAny` \e -> do
      log $ "could not load backup: " <> show e
      Database.new
  -- start the cursor at (0,0)
  cursor <- Cursor.origin db
  -- initialize storage by storing welcome message
  unless robotMode $
    Cursor.writeStrings cursor (TileRelativeCharCoord' 0 0) welcomeMessage
  pure db

save :: Logger -> Config.BackupConfig -> Database.DB -> IO ()
save log Config.BackupConfig {..} state = do
  log $ "save: backing up to " <> path
  -- database is first saved to a temporary file, then
  -- file is renamed to the final backup path to ensure
  -- atomic backup operations
  Database.save state tmp
  renameFile tmp path
  log "save: backup done"

backupDaemon :: Logger -> Config.BackupConfig -> Database.DB -> IO ()
backupDaemon log cfg state = do
  threadDelay 60000000 -- 1 minute (microseconds)
  log "backupDaemon: starting backup"
  save log cfg state `catchAny` \e ->
    log $ "backupDaemon: could not backup: " <> show e
  backupDaemon log cfg state
