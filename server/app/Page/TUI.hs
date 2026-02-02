{-# LANGUAGE NumericUnderscores #-}

module Page.TUI (startTUI) where

import Control.Exception (bracket, catch, finally)
import Control.Monad (unless, void)
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as B
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word64, Word8)
import Network.Socket
  ( AddrInfo (..),
    Socket,
    SocketOption (ReuseAddr),
    SocketType (Stream),
    accept,
    bind,
    close,
    defaultHints,
    getAddrInfo,
    listen,
    openSocket,
    setSocketOption,
    socketToHandle,
    withSocketsDo,
  )
import Page.Constants (block_size)
import qualified Page.Database as Database
import qualified Page.Database.Cursor as Cursor
import Page.Geometry (Area (..), Pinned (..), Rect (..))
import Page.Geometry.Coordinates (TileRelativeCharCoord' (..))
import System.IO
  ( BufferMode (NoBuffering),
    Handle,
    IOMode (ReadWriteMode),
    hClose,
    hFlush,
    hGetChar,
    hSetBuffering,
    hSetEncoding,
    hWaitForInput,
    latin1,
  )
import UnliftIO.Concurrent (forkIO)

-------------------------------------------------------------------------------
-- Types

data Key
  = KUp
  | KDown
  | KLeft
  | KRight
  | KPageUp
  | KPageDown
  | KEnter
  | KBackspace
  | KChar Char
  | KEsc
  | KCtrlC

data TUIState = TUIState
  { tCurX :: !Int,
    tCurY :: !Int,
    tScrollX :: !Int,
    tScrollY :: !Int,
    tWidth :: !Int,
    tViewH :: !Int,
    tCursor :: Cursor.Cursor,
    tCache :: Map (Int, Int) [Text],
    tDirty :: !Bool,
    tLastRev :: !Word64
  }

-------------------------------------------------------------------------------
-- Entry point

startTUI :: (String -> IO ()) -> Database.DB -> Int -> IO ()
startTUI infoLog db port = withSocketsDo $ do
  let hints = defaultHints {addrSocketType = Stream}
  addr : _ <- getAddrInfo (Just hints) (Just "0.0.0.0") (Just (show port))
  bracket (openSocket addr) close $ \sock -> do
    setSocketOption sock ReuseAddr 1
    bind sock (addrAddress addr)
    listen sock 5
    infoLog $ "TUI server listening on port " <> show port
    acceptLoop infoLog db sock

acceptLoop :: (String -> IO ()) -> Database.DB -> Socket -> IO ()
acceptLoop infoLog db sock = do
  (conn, _peer) <- accept sock
  void $ forkIO $ do
    h <- socketToHandle conn ReadWriteMode
    handleClient infoLog db h `finally` hClose h
  acceptLoop infoLog db sock

-------------------------------------------------------------------------------
-- Client handler

handleClient :: (String -> IO ()) -> Database.DB -> Handle -> IO ()
handleClient infoLog db h = do
  hSetBuffering h NoBuffering
  hSetEncoding h latin1

  -- Telnet negotiation: IAC WILL ECHO, IAC WILL SGA
  hPutBuilder h $
    B.word8 0xff <> B.word8 0xfb <> B.word8 0x01
      <> B.word8 0xff
      <> B.word8 0xfb
      <> B.word8 0x03
  hFlush h

  -- Hide cursor, clear screen
  hPutBuilder h $ esc "[?25l" <> esc "[2J" <> esc "[H"
  hFlush h

  -- Drain any remaining telnet negotiation bytes before probing size
  drainInput h

  -- Detect terminal size
  (termH, termW) <- detectTermSize h

  cursor <- Cursor.origin db
  lastRev <- Cursor.cRevision cursor

  let viewH = termH - 1

  cache <- queryViewport cursor 0 0 termW viewH
  let st0 =
        TUIState
          { tCurX = 0,
            tCurY = 0,
            tScrollX = 0,
            tScrollY = 0,
            tWidth = termW,
            tViewH = viewH,
            tCursor = cursor,
            tCache = cache,
            tDirty = True,
            tLastRev = lastRev
          }

  tickRef <- newIORef (0 :: Int)
  mainLoop infoLog h st0 tickRef `finally` cleanup h

cleanup :: Handle -> IO ()
cleanup h = do
  hPutBuilder h $ esc "[?25h" <> esc "[2J" <> esc "[H"
  hFlush h

-------------------------------------------------------------------------------
-- Main loop

mainLoop :: (String -> IO ()) -> Handle -> TUIState -> IORef Int -> IO ()
mainLoop infoLog h !st tickRef = do
  -- Render if dirty
  when_ (tDirty st) $ render h st
  let st' = st {tDirty = False}

  -- Poll for input with 50ms timeout
  ready <- hWaitForInput h 50
  if ready
    then do
      key <- readKey h
      case key of
        KEsc -> pure ()
        KCtrlC -> pure ()
        _ -> do
          st'' <- handleKey st' key
          st''' <- refetchIfScrolled st' st''
          mainLoop infoLog h st''' tickRef
    else do
      -- Tick counter for periodic refresh (~2s at 50ms per tick)
      tick <- readIORef tickRef
      let tick' = tick + 1
      writeIORef tickRef tick'
      st'' <-
        if tick' >= 40
          then do
            writeIORef tickRef 0
            refreshFromDB st'
          else pure st'
      mainLoop infoLog h st'' tickRef

-- | Re-fetch viewport blocks when scrolling changed the visible area
refetchIfScrolled :: TUIState -> TUIState -> IO TUIState
refetchIfScrolled old new_
  | tScrollX old == tScrollX new_ && tScrollY old == tScrollY new_ = pure new_
  | otherwise = do
      cache' <- queryViewport (tCursor new_) (tScrollX new_) (tScrollY new_) (tWidth new_) (tViewH new_)
      pure new_ {tCache = cache', tDirty = True}

when_ :: Bool -> IO () -> IO ()
when_ True act = act
when_ False _ = pure ()

-------------------------------------------------------------------------------
-- Rendering

render :: Handle -> TUIState -> IO ()
render h TUIState {tCurX, tCurY, tScrollX, tScrollY, tWidth, tViewH, tCache} = do
  hPutBuilder h $ renderScreen tCurX tCurY tScrollX tScrollY tWidth tViewH tCache
  hFlush h

renderScreen ::
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Map (Int, Int) [Text] ->
  Builder
renderScreen curX curY scrollX scrollY termW viewH cache =
  esc "[?25l"
    <> mconcat [renderRow row | row <- [0 .. viewH - 1]]
    <> renderStatusBar
  where
    scrCurX = curX - scrollX
    scrCurY = curY - scrollY

    renderRow screenRow =
      esc "["
        <> B.intDec (screenRow + 1)
        <> B.string7 ";1H"
        <> mconcat [renderCell screenRow col | col <- [0 .. termW - 1]]

    renderCell screenRow screenCol =
      let charX = scrollX + screenCol
          charY = scrollY + screenRow
          ch = lookupChar charX charY cache
          isCursor = screenRow == scrCurY && screenCol == scrCurX
       in if isCursor
            then esc "[7m" <> B.char7 ch <> esc "[0m"
            else B.char7 ch

    renderStatusBar =
      let pos = "(" <> show curX <> "," <> show curY <> ")"
          label = " page TUI "
          pad = max 0 (termW - length label - length pos)
          bar = take termW (label <> replicate pad ' ' <> pos)
       in esc "["
            <> B.intDec (viewH + 1)
            <> B.string7 ";1H"
            <> esc "[7m"
            <> B.string7 bar
            <> esc "[0m"

lookupChar :: Int -> Int -> Map (Int, Int) [Text] -> Char
lookupChar cx cy cache =
  let bx = divFloor cx block_size
      by = divFloor cy block_size
      lx = cx - bx * block_size
      ly = cy - by * block_size
   in case Map.lookup (bx, by) cache of
        Nothing -> ' '
        Just rows
          | ly < 0 || ly >= length rows -> ' '
          | otherwise ->
              let row = rows !! ly
               in if lx < 0 || lx >= T.length row
                    then ' '
                    else
                      let c = T.index row lx
                       in if c == '\0' then ' ' else c

divFloor :: Int -> Int -> Int
divFloor a b
  | b > 0 = if a >= 0 then a `div` b else -(((-a) + b - 1) `div` b)
  | otherwise = error "divFloor: non-positive divisor"

-------------------------------------------------------------------------------
-- Terminal size detection

-- | Drain any pending input (e.g. telnet negotiation responses).
drainInput :: Handle -> IO ()
drainInput h = do
  ready <- hWaitForInput h 200
  if ready
    then do
      _ <- hGetByte h
      drainInput h
    else pure ()

-- | Detect terminal size by moving cursor to a large position
-- and requesting the cursor position report. Falls back to 80x24.
detectTermSize :: Handle -> IO (Int, Int)
detectTermSize h = do
  -- Move cursor far away, then ask "where are you?"
  hPutBuilder h $ esc "[999;999H" <> esc "[6n"
  hFlush h
  -- Response is \ESC[rows;colsR
  parseCPR h `catchIOError` \_ -> pure (24, 80)

-- | Parse a Cursor Position Report: \ESC [ rows ; cols R
parseCPR :: Handle -> IO (Int, Int)
parseCPR h = do
  -- Read until we get ESC
  let waitESC = do
        ready <- hWaitForInput h 1000
        if not ready
          then pure (24, 80)
          else do
            b <- hGetByte h
            case b of
              0xff -> consumeIAC h >> waitESC -- skip any straggling IAC
              0x1b -> do
                bracket_ <- hGetByte h -- should be '['
                if bracket_ == 0x5b
                  then do
                    rows <- readNum h
                    cols <- readNum h
                    pure (max 1 rows, max 1 cols)
                  else pure (24, 80)
              _ -> waitESC -- skip other bytes
  waitESC

-- | Read a decimal number from the handle, terminated by ';' or 'R'.
readNum :: Handle -> IO Int
readNum h = go 0
  where
    go !acc = do
      b <- hGetByte h
      if b >= 0x30 && b <= 0x39 -- '0'-'9'
        then go (acc * 10 + fromIntegral (b - 0x30))
        else pure acc -- ';' or 'R' terminates

catchIOError :: IO a -> (IOError -> IO a) -> IO a
catchIOError = catch

-------------------------------------------------------------------------------
-- Input

readKey :: Handle -> IO Key
readKey h = do
  c <- hGetByte h
  case c of
    0xff -> consumeIAC h >> readKey h -- telnet negotiation; skip and retry
    0x1b -> readEscSeq h
    0x03 -> pure KCtrlC
    0x0d -> pure KEnter
    0x7f -> pure KBackspace
    0x08 -> pure KBackspace
    _
      | c >= 32 && c <= 126 -> pure $ KChar (toEnum $ fromIntegral c)
      | otherwise -> readKey h -- skip unknown control bytes

-- | Consume a telnet IAC command (the 0xFF was already read).
-- DO/DONT/WILL/WONT are 3-byte sequences (IAC + cmd + option).
-- Sub-negotiation (IAC SB ... IAC SE) is consumed until IAC SE.
consumeIAC :: Handle -> IO ()
consumeIAC h = do
  cmd <- hGetByte h
  case cmd of
    0xfa -> consumeSubNeg h -- SB: sub-negotiation, read until IAC SE
    _ -> void $ hGetByte h -- DO/DONT/WILL/WONT + option byte

consumeSubNeg :: Handle -> IO ()
consumeSubNeg h = do
  b <- hGetByte h
  if b == 0xff
    then do
      b2 <- hGetByte h
      unless (b2 == 0xf0) $ consumeSubNeg h -- 0xf0 = SE, end
    else consumeSubNeg h

readEscSeq :: Handle -> IO Key
readEscSeq h = do
  ready <- hWaitForInput h 50
  if not ready
    then pure KEsc
    else do
      c <- hGetByte h
      if c == 0x5b -- '['
        then readCSI h
        else pure KEsc

readCSI :: Handle -> IO Key
readCSI h = do
  c <- hGetByte h
  case c of
    0x41 -> pure KUp
    0x42 -> pure KDown
    0x43 -> pure KRight
    0x44 -> pure KLeft
    0x35 -> do
      -- Page Up: '5~'
      void $ hGetByte h
      pure KPageUp
    0x36 -> do
      -- Page Down: '6~'
      void $ hGetByte h
      pure KPageDown
    _ -> pure KEsc

hGetByte :: Handle -> IO Word8
hGetByte h = fromIntegral . fromEnum <$> hGetChar h

-------------------------------------------------------------------------------
-- Key handling

handleKey :: TUIState -> Key -> IO TUIState
handleKey st key = case key of
  KUp -> pure $ moveCursor st 0 (-1)
  KDown -> pure $ moveCursor st 0 1
  KLeft -> pure $ moveCursor st (-1) 0
  KRight -> pure $ moveCursor st 1 0
  KPageUp -> pure $ pageScroll st (-(tViewH st))
  KPageDown -> pure $ pageScroll st (tViewH st)
  KEnter -> pure st {tCurX = 0, tCurY = tCurY st + 1, tDirty = True}
  KBackspace -> do
    let st' = moveCursor st (-1) 0
    Cursor.writeCharAt (tCursor st') (TileRelativeCharCoord' (tCurX st') (tCurY st')) ' '
    let cache' = updateCacheChar (tCurX st') (tCurY st') ' ' (tCache st')
    pure st' {tCache = cache', tDirty = True}
  KChar c -> do
    Cursor.writeCharAt (tCursor st) (TileRelativeCharCoord' (tCurX st) (tCurY st)) c
    let cache' = updateCacheChar (tCurX st) (tCurY st) c (tCache st)
        st' = st {tCache = cache', tCurX = tCurX st + 1, tDirty = True}
    pure $ autoScroll st'
  _ -> pure st

moveCursor :: TUIState -> Int -> Int -> TUIState
moveCursor st dx dy =
  autoScroll st {tCurX = tCurX st + dx, tCurY = tCurY st + dy, tDirty = True}

autoScroll :: TUIState -> TUIState
autoScroll st =
  let sx = tScrollX st
      sy = tScrollY st
      cx = tCurX st
      cy = tCurY st
      w = tWidth st
      vh = tViewH st
      sx'
        | cx < sx = cx
        | cx >= sx + w = cx - w + 1
        | otherwise = sx
      sy'
        | cy < sy = cy
        | cy >= sy + vh = cy - vh + 1
        | otherwise = sy
   in if sx' /= sx || sy' /= sy
        then st {tScrollX = sx', tScrollY = sy'}
        else st

pageScroll :: TUIState -> Int -> TUIState
pageScroll st dy =
  st {tScrollY = tScrollY st + dy, tCurY = tCurY st + dy, tDirty = True}

updateCacheChar :: Int -> Int -> Char -> Map (Int, Int) [Text] -> Map (Int, Int) [Text]
updateCacheChar cx cy ch cache =
  let bx = divFloor cx block_size
      by = divFloor cy block_size
      lx = cx - bx * block_size
      ly = cy - by * block_size
   in case Map.lookup (bx, by) cache of
        Nothing -> cache
        Just rows
          | ly < 0 || ly >= length rows -> cache
          | otherwise ->
              let row = rows !! ly
                  row' = T.take lx row <> T.singleton ch <> T.drop (lx + 1) row
                  rows' = take ly rows ++ [row'] ++ drop (ly + 1) rows
               in Map.insert (bx, by) rows' cache

-------------------------------------------------------------------------------
-- Database interaction

queryViewport :: Cursor.Cursor -> Int -> Int -> Int -> Int -> IO (Map (Int, Int) [Text])
queryViewport cursor scrollX scrollY termW viewH = do
  let x1 = divFloor scrollX block_size
      y1 = divFloor scrollY block_size
      x2 = divFloor (scrollX + termW - 1) block_size + 1
      y2 = divFloor (scrollY + viewH - 1) block_size + 1
      area = Area [Rect x1 y1 (x2 + 1) (y2 + 1)]
  regions <- Cursor.query cursor area
  pure $ foldl addRegion Map.empty regions

addRegion :: Map (Int, Int) [Text] -> Pinned Int Database.Block -> Map (Int, Int) [Text]
addRegion cache (Pinned bx by block) =
  Map.insert (bx, by) (Database.blockToTexts block) cache

refreshFromDB :: TUIState -> IO TUIState
refreshFromDB st = do
  newRev <- Cursor.cRevision (tCursor st)
  if newRev == tLastRev st
    then pure st
    else do
      cache' <- queryViewport (tCursor st) (tScrollX st) (tScrollY st) (tWidth st) (tViewH st)
      pure st {tCache = cache', tLastRev = newRev, tDirty = True}

-------------------------------------------------------------------------------
-- Helpers

esc :: String -> Builder
esc s = B.char7 '\ESC' <> B.string7 s

hPutBuilder :: Handle -> Builder -> IO ()
hPutBuilder = B.hPutBuilder
