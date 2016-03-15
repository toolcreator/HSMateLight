{-# LANGUAGE GeneralizedNewtypeDeriving,RankNTypes,ScopedTypeVariables #-}
module MateLight (
   EventProvider
  ,parseAddress
  ,Config(..)
  ,runMate
  ,runMateM
  ,Frame(..)
  ,Event(..)
  ) where
import Data.Word
import System.IO
import Control.Monad.State
import Control.Monad.Reader
import Data.IP
import qualified Network.Socket as Sock
import qualified Network.Socket.ByteString.Lazy as NBSL
import qualified Data.ByteString.Lazy as BSL
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan

class Frame f where
  theData :: f -> BSL.ByteString
  dimension :: f -> (Int, Int)

data Event = Event String String deriving (Eq, Ord, Show, Read)

type EventProvider = TChan Event -> IO ()

data Config = Config {
   cAddr :: IP
  ,cPort :: Sock.PortNumber
  ,cDimension :: (Int, Int)
  ,cStepTime :: Maybe Int
  ,cSynchronized :: Bool -- aggregate events, send only at step
  ,cEventProviders :: [EventProvider]
}

parseAddress :: String -> Maybe IP
parseAddress str = maybe (IPv4 `fmap` (readMaybe str :: Maybe IPv4)) (fmap IPv6) (readMaybe str)
  where readMaybe str = case reads str of { [(a, "")] -> Just a; _ -> Nothing }

runMate :: (Frame f) => Config -> ((Int, Int) -> [Event] -> s -> (f, s)) -> s -> IO ()
runMate conf fkt stat = runMateM conf ((state .) . fkt) stat

newtype MateMonad f s m a = MateMonad {
  unMateMonad :: (StateT s (ReaderT f m) a)
  } deriving (Applicative, Functor, Monad, MonadIO, MonadState s, MonadReader f)

whileM :: Monad m => m Bool -> m a -> m [a]
whileM cond action = do
  c <- cond
  if c then do
    res <- action
    ress <- whileM cond action
    return $ res : ress
   else
    return []

runMateM :: forall f s . (Frame f) => Config -> ((Int, Int) -> [Event] -> MateMonad f s IO f) -> s -> IO ()
runMateM conf fkt s = do
  -- Change socket code
  sock <- Sock.socket Sock.AF_INET Sock.Datagram Sock.defaultProtocol 
  case cAddr conf of
    IPv4 ip -> Sock.connect sock $ Sock.SockAddrInet (cPort conf) (toHostAddress ip)
    IPv6 ip -> Sock.connect sock $ Sock.SockAddrInet6 (cPort conf) 0 (toHostAddress6 ip) 0
  chanStepper <- newChan :: IO (Chan ())
  case cStepTime conf of
    Nothing -> return ()
    Just time -> dupChan chanStepper >>= forkIO . stepper time >> return ()
  chanEvent <- newTChanIO :: IO (TChan Event)
  forM_ (cEventProviders conf) $ \ep -> atomically (dupTChan chanEvent) >>= forkIO . ep
  forkIO $ (if cSynchronized conf then acumulatingCaller else caller) sock chanStepper chanEvent undefined s
  do -- this whole do block stinks, but then so does capturing keyboard events
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    atomically (dupTChan chanEvent) >>= \ch -> forever $ do
      c <- getChar
      atomically $ writeTChan ch $ Event "KEYBOARD" [c]
  Sock.close sock
  where
  stepper :: Int -> Chan () -> IO ()
  stepper delay chan = do
    writeChan chan ()
    threadDelay delay
    stepper delay chan
  acumulatingCaller :: Sock.Socket -> Chan () -> TChan Event -> f -> s -> IO ()
  acumulatingCaller sock chanStepper chanEvent oldFrame curState = do
    () <- readChan chanStepper
    events <- atomically $ whileM (not `fmap` isEmptyTChan chanEvent) (readTChan chanEvent)
    (newFrame, newState) <- runReaderT (runStateT (unMateMonad (fkt (cDimension conf) events)) curState) oldFrame :: IO (f, s)
    sendFrame sock newFrame
    acumulatingCaller sock chanStepper chanEvent newFrame newState
  caller :: Sock.Socket -> Chan () -> TChan Event -> f -> s -> IO ()
  caller sock chanStepper chanEvent oldFrame oldState = do
    unitedChan <- newChan :: IO (Chan (Either () Event))
    dupChan unitedChan >>= \dupped -> forkIO $ forever $ do
      () <- readChan chanStepper
      writeChan dupped $ Left ()
    dupChan unitedChan >>= \dupped -> forkIO $ forever $ do
      event <- atomically $ readTChan chanEvent
      writeChan dupped $ Right event
    let helper oldFrame curState = do
          msg <- readChan unitedChan
          (newFrame, newState) <- runReaderT (runStateT (unMateMonad (fkt (cDimension conf) [ev | Right ev <- [msg]])) curState) oldFrame :: IO (f, s)
          sendFrame sock newFrame
          helper newFrame newState
    helper oldFrame oldState
  sendFrame :: Frame f => Sock.Socket -> f -> IO ()
  sendFrame sock frame = if BSL.length bs == fromIntegral (x * y * 3) then NBSL.sendAll sock $ theData frame else hPutStrLn stderr "function returned incorrect frame"
    where
    (x, y) = cDimension conf
    bs = theData frame
