{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main where
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.WebSockets as WSock
import qualified Network.Wai.Handler.WebSockets as WWSock
import qualified Network.Socket as Sock
import qualified Network.Socket.ByteString as NBS
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TChan
import Control.Exception
import Control.DeepSeq
import System.IO.Unsafe (unsafePerformIO)
import Data.Maybe
import Data.Foldable
import System.IO (Handle, hPutStrLn, stdin, stdout, stderr, hSetBuffering, BufferMode(NoBuffering))
import qualified System.Posix.Signals as Signals

import Types
import StaticFiles
import CmdArgs
import qualified StaticPages
import qualified DropPriv as Priv

{-# NOINLINE logLock #-}
logLock :: MVar ()
logLock = unsafePerformIO $ newMVar ()

mHPutStrLn :: Handle -> String -> IO ()
mHPutStrLn handle str = withMVar logLock (\() -> hPutStrLn handle str)
mPutStrLn = mHPutStrLn stdout

newDisplay :: IO [(Bool, Sock.SockAddr)] -> (TVar (Maybe (TChan Frame)) -> IO ()) -> IO () -> WSock.ServerApp
newDisplay clients addDisplay doClose pendConn = do
  mHPutStrLn stderr "newDisplay started"
  wconnMb <- (WSock.acceptRequest pendConn >>= return . Just) `catches` [
            Handler $ \(e :: WSock.HandshakeException) -> mHPutStrLn stderr ("exception at acceptRequest (HandshakeException)" ++ show e) >> doClose >> return Nothing
           ,Handler $ \(e :: WSock.ConnectionException) -> mHPutStrLn stderr ("exception at acceptRequest (ConnectionException)" ++ show e) >> doClose >> return Nothing
           ,Handler $ \(e :: SomeException) -> mHPutStrLn stderr ("exception at standalone acceptRequest (SomeException)" ++ show e) >> doClose >> return Nothing
           ]
  case wconnMb of
    Nothing -> doClose
    Just wconn -> do
      chan <- newTChanIO
      mvchan <- newTVarIO $ Just chan
      activeAddress <- clients >>= \cs -> case cs of { [(_, c)] -> newTVarIO $ Just c; _ -> newTVarIO Nothing }
      tidrMVar <- newEmptyMVar
      tidsMVar <- newEmptyMVar
      mPutStrLn "got connection"
      sendLock <- newMVar ()
      let wSend msg = withMVar sendLock (\() -> WSock.sendBinaryData wconn (msg :: Message))
      tidUpdater <- myThreadId

      let receiver = handle (\(e :: WSock.ConnectionException) -> mPutStrLn ("exception in receiver: " ++ show e)) $ flip finally (closeMVar tidsMVar) $ do
            msg <- WSock.receive wconn
            case msg of
              WSock.ControlMessage (WSock.Close _ _) -> mPutStrLn "received close message" >> closeMVar tidsMVar
              WSock.DataMessage (WSock.Binary bs) | isInetBS bs || isInet6BS bs -> do
                let addr = WSock.fromLazyByteString bs :: Sock.SockAddr
                mPutStrLn $ "received update: " ++ show addr
                atomically $ writeTVar activeAddress $ Just addr
                threadDelay 10000
                receiver
              _ -> mPutStrLn "unrecognized message received" >> receiver
          sender = handle (\(e :: WSock.ConnectionException) -> mPutStrLn ("exception in sender: " ++ show e)) $ flip finally (closeMVar tidrMVar) $ do
            msg <- atomically $ readTChan chan
            sockAddr <- atomically $ readTVar activeAddress
            when (sockAddr == Just (source msg)) $ wSend $ MFrame msg
            sender
          closeMVar mvar = do
            tid <- readMVar mvar
            atomically $ modifyTVar mvchan (\_ -> Nothing)
            killThread tid
            killThread tidUpdater
            doClose

      forkIO receiver >>= putMVar tidrMVar
      forkIO sender >>= putMVar tidsMVar
      addDisplay mvchan
      forever $ do
        cs <- clients
        wSend $ MCUpdate $ map snd cs
        threadDelay 1000000

receive :: Sock.Socket -> IO Frame
receive udpSocket = uncurry (flip Frame) `fmap` NBS.recvFrom udpSocket 2048

-- Use System.Timeout
runMateEmu :: MateArgs ->  IO ()
runMateEmu margs = bracket (mkSock Sock.Datagram (ip margs) (mateport margs)) Sock.close $ \udpSocket -> bracket (mkSock Sock.Stream (ip margs) (port margs)) Sock.close $ \websocket -> do
  Sock.listen websocket 5
  {-Sock.setSocketOption websocket Sock.ReuseAddr 1-}
  {-Sock.setSocketOption websocket Sock.NoDelay 1-}
  continue <- newTVarIO True
  when (isJust $ uidgid margs) $ do
    Priv.dropUidGid (Left $ fst $ fromJust $ uidgid margs) (Left $ snd $ fromJust $ uidgid margs)
    Priv.status >>= \stat -> mPutStrLn $ "dropped privs to: " ++ show stat
  _ <- Signals.installHandler Signals.sigINT (Signals.CatchOnce $ atomically $ writeTVar continue False) Nothing
  _ <- Signals.installHandler Signals.sigTERM (Signals.CatchOnce $ atomically $ writeTVar continue False) Nothing
  displays <- newTVarIO []
  clients <- newTVarIO []
  let websocketPage = newDisplay (atomically $ readTVar clients) (\display -> atomically $ modifyTVar displays (display :)) (return ())
  forkIO $ Warp.runSettingsSocket (Warp.setPort 8080 Warp.defaultSettings) websocket $ WWSock.websocketsOr WSock.defaultConnectionOptions websocketPage mainPage
  masterChan <- newChan
  forkIO $ forever $ do
    bs <- readChan masterChan
    atomically $ send displays bs
  forkIO $ forever $ do
    msg <- receive udpSocket
    atomically $ touchSeen clients $ source msg
    writeChan masterChan msg
  forkIO $ forever $ do
    threadDelay 500000
    atomically $ cleanupDisplays displays
    atomically $ cleanupClients clients
  let loop disps' cs' = atomically (readTVar continue) >>= \c -> when c $ do
                disps <- atomically $ readTVar displays
                cs <- atomically $ readTVar clients
                when (disps' /= disps || cs /= cs') $ mPutStrLn $ "number of displays = " ++ show (length disps) ++ "\nnumber of clients = " ++ show (length cs) ++ "\nclients = " ++ show cs
                threadDelay 500000
                loop disps cs
  loop [] []
  where
  send displays bs = readTVar displays >>= foldrM (\d acc -> readTVar d >>= maybe (return acc) (\c -> writeTChan c bs >> return (d : acc))) [] >>= writeTVar displays
  cleanupDisplays displays = readTVar displays >>= filterM (fmap isJust . readTVar) >>= writeTVar displays
  cleanupClients clients = modifyTVar clients $ concatMap (\(seen, addr) -> [(False, addr) | seen])
  touchSeen clients sockAddr = modifyTVar clients helper
    where
    helper [] = [(True, sockAddr)]
    helper (c@(seen, addr):cs) | addr == sockAddr = (True, addr) : cs
                               | otherwise = c : helper cs
  mkSock ptype addr port = do
    socket <- Sock.socket Sock.AF_INET ptype Sock.defaultProtocol
    addr <- Sock.inet_addr addr
    Sock.bind socket (Sock.SockAddrInet (fromIntegral port) addr)
    return socket
  mainPage = StaticPages.staticPages staticFiles Nothing (StaticPages.lookupWithDefault ["index.html"])

main :: IO ()
main = do
  args <- getMateArgs
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering
  putStr "preparing... "
  staticFiles `deepseq` putStrLn "done"
  putStrLn $ "open http://" ++ ip args ++ ":" ++ show (port args) ++ "\nand stream crap to " ++ ip args ++ ":" ++ show (mateport args) ++ " udp"
  Sock.withSocketsDo (runMateEmu args)
