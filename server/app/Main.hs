{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}

module Main (main, main') where

import Control.Monad (forM_, unless, when)
import Data.Aeson (eitherDecode, encode)
import qualified Data.ByteString.Char8 as BS8
import Data.ByteString.Lazy (ByteString)
import Data.List.Split (splitOn)
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
import System.Log.FastLogger (defaultBufSize, newStderrLoggerSet, pushLogStrLn, toLogStr)
import UnliftIO (catchAny, readIORef, throwIO, withAsync)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Directory (renameFile)

main :: IO ()
main = do
  configPath <- OptParse.execParser opts
  main' configPath
  where
    opts = OptParse.info (parser OptParse.<**> OptParse.helper) OptParse.fullDesc
    parser =
      OptParse.argument OptParse.str $
        OptParse.metavar "PATH"
          <> OptParse.help "Path to configuration file"

-- | Like 'main', but the path to the configuration
-- file is passed as an argument.
main' :: FilePath -> IO ()
main' fp = do
  loggerSet <- newStderrLoggerSet defaultBufSize
  let infoLog = pushLogStrLn loggerSet . toLogStr
  configResult <- Config.open fp
  case configResult of
    Left err -> do
      infoLog $ "error opening config file: " ++ show err
      ioError $ userError $ "failed to load configuration at " <> fp
    Right cfg -> do
      let port = Config.port $ Config.server cfg
          backupCfg = Config.backup cfg
          robotMode = Config.robots cfg
      db <- load infoLog backupCfg robotMode
      let telnet = case Config.telnetPort cfg of
            Just p -> TUI.startTUI infoLog db p
            Nothing -> pure ()
      withAsync telnet $ \_ ->
        withAsync (backupDaemon infoLog backupCfg db) $ \_ ->
          WS.runServer "0.0.0.0" port $ application infoLog cfg db

-- | Security checks on incoming connection
verify :: Config.CfTurnstileConfig -> WS.PendingConnection -> IO (Security.Result ())
verify cfg pending = flip catchAny (\e -> print ("verify: " <> show e) >> throwIO e) $ do
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
            ["page.acondolu.me", token, idempotencyKey] ->
              Security.verifyToken secretKey token remoteIP idempotencyKey
            _ -> pure $ Security.Error "Invalid Sec-WebSocket-Protocol"
        _ -> pure $ Security.Error "Sec-WebSocket-Protocol missing"

type Logger = String -> IO ()

application :: Logger -> Config.Config -> Database.DB -> WS.ServerApp
application infoLog config db pending = do
  let robotMode = Config.robots config
  result <- if robotMode
    then pure $ Security.Success ()
    else verify (Config.cfTurnstile config) pending
  case result of
    s@(Security.Error str) -> do
      infoLog $ show s
      WS.rejectRequest pending $ BS8.pack str
    Security.Success () -> do
      conn <- WS.acceptRequestWith
            pending
            WS.defaultAcceptRequest
              { WS.acceptHeaders =
                  [ ("Sec-WebSocket-Protocol", "page.acondolu.me")
                  ]
              }
      handleConnection infoLog robotMode db conn `catchAny` \e ->
        infoLog $ "handleConnection: " <> show e

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
    strokes :: Security.StrokeLimiter
  }

viewport :: ConnState -> Geometry.Rect Int
viewport ConnState {w, h}
  | w == 0 || h == 0 = Geometry.Rect 0 0 0 0
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

handleConnection :: Logger -> Bool -> Database.DB -> WS.Connection -> IO ()
handleConnection infoLog robotMode db conn = do
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
        strokes = Security.newStrokeLimiter
      }
  where
    loop !state = do
      cmd <- recv conn
      infoLog $ "command received: " <> show cmd
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

    -- send new blocks visible due to change of viewport
    sendDiff state state' | emptyViewport state' = do
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
          infoLog "pong"
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
load infoLog Config.BackupConfig {path} robotMode = do
  db <-
    Database.load path `catchAny` \e -> do
      infoLog $ "could not load backup: " <> show e
      Database.new
  -- start the cursor at (0,0)
  cursor <- Cursor.origin db
  -- initialize storage by storing welcome message
  unless robotMode $
    Cursor.writeStrings cursor (TileRelativeCharCoord' 0 0) welcomeMessage
  pure db

save :: Logger -> Config.BackupConfig -> Database.DB -> IO ()
save infoLog Config.BackupConfig {..} state = do
  infoLog $ "save: backing up to " <> path
  -- database is first saved to a temporary file, then
  -- file is renamed to the final backup path to ensure
  -- atomic backup operations
  Database.save state tmp
  renameFile tmp path
  infoLog "save: backup done"

backupDaemon :: Logger -> Config.BackupConfig -> Database.DB -> IO ()
backupDaemon infoLog cfg state = do
  threadDelay 60000000 -- 1 minute (microseconds)
  infoLog "backupDaemon: starting backup"
  save infoLog cfg state `catchAny` \e ->
    infoLog $ "backupDaemon: could not backup: " <> show e
  backupDaemon infoLog cfg state
