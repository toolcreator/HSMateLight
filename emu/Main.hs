{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
import qualified Network.WebSockets as WSock
import qualified Network.WebSockets.Stream as WStream
import qualified Network.Socket as Sock
import qualified Network.Socket.ByteString as NBS
import qualified Data.ByteString as BS
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TChan
import Control.Exception
import System.IO.Unsafe (unsafePerformIO)
import Data.Maybe
import Data.Foldable
import System.IO (Handle, hPutStrLn, stdin, stdout, stderr)

logLock :: MVar ()
logLock = unsafePerformIO $ newMVar ()

mHPutStrLn :: Handle -> String -> IO ()
mHPutStrLn handle str = withMVar logLock (\() -> hPutStrLn handle str)
mPutStrLn = mHPutStrLn stdout

data Message = Message {
   source :: Sock.SockAddr
  ,message :: BS.ByteString
} deriving Show

mAccept :: Sock.Socket -> IO (TVar (Maybe (TChan Message)))
mAccept websocket = do
  (conn, _) <- Sock.accept websocket
  pending <- WSock.makePendingConnection conn (WSock.ConnectionOptions (return ()))
  chan <- newTChanIO
  mvchan <- newTVarIO $ Just chan
  tidrMVar <- newEmptyMVar
  tidsMVar <- newEmptyMVar
  mPutStrLn "got connection"
  wconn <- WSock.acceptRequest pending

  let receiver = handle (\(e :: WSock.ConnectionException) -> mPutStrLn ("received exception in receiver: " ++ show e ++ " -> closing") >> closeMVar tidsMVar) $ do
        msg <- WSock.receive wconn
        mPutStrLn $ "received message " ++ show msg
        case msg of
          WSock.ControlMessage (WSock.Close _ _) -> closeMVar tidsMVar
          _ -> receiver
      sender = handle (\(e :: WSock.ConnectionException) -> mPutStrLn ("received exception in sender: " ++ show e ++ " -> closing") >> closeMVar tidrMVar) $ do
        bs <- atomically $ readTChan chan
        WSock.sendBinaryData wconn $ message bs -- client can filter here by socksaddr
        sender
      closeMVar mvar = do
        tid <- readMVar mvar
        atomically $ modifyTVar mvchan (\_ -> Nothing)
        killThread tid
        Sock.close conn

  tidr <- forkIO receiver
  putMVar tidrMVar tidr
  tids <- forkIO sender
  putMVar tidsMVar tids
  return mvchan

receive :: Sock.Socket -> IO Message
receive udpSocket = uncurry (flip Message) `fmap` NBS.recvFrom udpSocket 4096

-- use bracket for close...
main :: IO ()
main = Sock.withSocketsDo $ bracket mkSocks (\(a, b) -> Sock.close a >> Sock.close b) $ \(websocket, udpSocket) -> do
  displays <- newTVarIO []
  clients <- newTVarIO []
  forkIO $ forever $ do
    display <- mAccept websocket
    atomically $ modifyTVar displays (display :)
  masterChan <- newChan
  forkIO $ forever $ do
    bs <- readChan masterChan
    {-mPutStrLn "processing message"-}
    atomically $ send displays bs
  forkIO $ forever $ do
    msg <- receive udpSocket
    atomically $ touchSeen clients $ source msg
    writeChan masterChan msg
  forkIO $ forever $ do
    threadDelay 3000000
    mPutStrLn "cleaning up"
    atomically $ cleanupDisplays displays
    atomically $ cleanupClients clients
  forever $ do
    disps <- atomically $ readTVar displays
    cs <- atomically $ readTVar clients
    mPutStrLn $ "number of displays = " ++ show (length disps) ++ "\nnumber of clients = " ++ show (length cs)
    threadDelay 1000000
  {-
   -forever $ do
   -  let black = Message undefined $ BS.pack $ replicate (40*16*3) 0
   -      white = Message undefined $ BS.pack $ replicate (40*16*3) 0xff
   -  threadDelay 50000
   -  writeChan masterChan black
   -  threadDelay 50000
   -  writeChan masterChan white
   -}
  where
  send displays bs = readTVar displays >>= foldrM (\d acc -> readTVar d >>= maybe (return acc) (\c -> writeTChan c bs >> return (d : acc))) [] >>= writeTVar displays
  cleanupDisplays displays = readTVar displays >>= filterM (fmap isJust . readTVar) >>= writeTVar displays
  cleanupClients clients = modifyTVar clients $ concatMap (\(seen, addr) -> [(False, addr) | seen])
  touchSeen clients sockAddr = modifyTVar clients helper
    where
    helper [] = [(True, sockAddr)]
    helper (c@(seen, addr):cs) | addr == sockAddr = (True, addr) : cs
                               | otherwise = c : helper cs
  mkSocks = do
    udpSocket <- Sock.socket Sock.AF_INET Sock.Datagram Sock.defaultProtocol
    addr <- Sock.inet_addr "127.0.0.1"
    Sock.bind udpSocket (Sock.SockAddrInet (fromIntegral 1337) addr)
    websocket <- WSock.makeListenSocket "127.0.0.1" 8080
    return (websocket, udpSocket)
