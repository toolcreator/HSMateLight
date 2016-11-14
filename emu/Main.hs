{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
import qualified Network.WebSockets as WSock
import qualified Network.WebSockets.Stream as WStream
import qualified Network.Socket as Sock
import qualified Network.Socket.ByteString as NBS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TChan
import Control.Exception
import System.IO.Unsafe (unsafePerformIO)
import Data.Maybe
import Data.Foldable
import System.IO (Handle, hPutStrLn, stdin, stdout, stderr, hSetBuffering, BufferMode(NoBuffering))
import Data.Bits
import Data.Word
import Test.QuickCheck
import qualified DropPriv as Priv
import qualified System.Posix.Signals as Signals

{-# NOINLINE logLock #-}
logLock :: MVar ()
logLock = unsafePerformIO $ newMVar ()

mHPutStrLn :: Handle -> String -> IO ()
mHPutStrLn handle str = withMVar logLock (\() -> hPutStrLn handle str)
mPutStrLn = mHPutStrLn stdout

data Frame = Frame {
   source :: Sock.SockAddr
  ,message :: BS.ByteString
} deriving (Show, Eq)

instance Arbitrary Sock.SockAddr where
  arbitrary = oneof [inet, inet6]
    where
    inet = arbitrary >>= \(pn :: Word16) -> arbitrary >>= \ha -> return $ Sock.SockAddrInet (fromIntegral pn) ha
    inet6 = arbitrary >>= \(pn :: Word16) -> arbitrary >>= \fi -> arbitrary >>= \ha -> arbitrary >>= \si -> return $ Sock.SockAddrInet6 (fromIntegral pn) fi ha si
instance Arbitrary Message where -- tests only MCUpdate
  arbitrary = MCUpdate `fmap` arbitrary
prop_sockLength sa@(Sock.SockAddrInet _ _) = BSL.length (WSock.toLazyByteString sa) == 7
prop_sockLength sa@(Sock.SockAddrInet6 _ _ _ _) = BSL.length (WSock.toLazyByteString sa) == 27
prop_sock sa = WSock.fromLazyByteString (WSock.toLazyByteString sa) == (sa :: Sock.SockAddr)
prop_sockAddrLst msg = WSock.fromLazyByteString (WSock.toLazyByteString msg) == (msg :: Message)

data Message = MFrame Frame | MCUpdate [Sock.SockAddr] deriving (Show, Eq)
instance WSock.WebSocketsData Message where
  fromLazyByteString bs | BSL.length bs > 0 && BSL.head bs == 0 = MFrame $ Frame (error "no sockAddr") $ BSL.toStrict $ BSL.tail bs
  fromLazyByteString bs | BSL.length bs > 0 && BSL.head bs == 1 = MCUpdate $ map WSock.fromLazyByteString $ split tl
    where
    tl = BSL.tail bs
    split bs | BSL.null bs = []
            | BSL.length bs >= 7 && BSL.head bs == 0 = BSL.take 7 bs : split (BSL.drop 7 bs)
            | BSL.length bs >= 27 && BSL.head bs == 1 = BSL.take 27 bs : split (BSL.drop 27 bs)
  toLazyByteString (MFrame frame) = 0 `BSL.cons` BSL.fromStrict (message frame)
  toLazyByteString (MCUpdate clients) = 1 `BSL.cons` BSL.concat (map WSock.toLazyByteString clients)

isInetBS :: BSL.ByteString -> Bool
isInetBS bs = BSL.length bs == 7 && BSL.head bs == 0
isInet6BS :: BSL.ByteString -> Bool
isInet6BS bs = BSL.length bs == 27 && BSL.head bs == 1

instance WSock.WebSocketsData Sock.SockAddr where
  fromLazyByteString bs | isInetBS bs = Sock.SockAddrInet pn ha
    where
    [pn1, pn0, ha3, ha2, ha1, ha0] = BSL.unpack $ BSL.tail bs
    pn = fromIntegral (bytesToWord $ [pn1, pn0] :: Word16)
    ha = bytesToWord [ha3, ha2, ha1, ha0]
  fromLazyByteString bs | isInet6BS bs = Sock.SockAddrInet6 pn fi ha si
    where
    rs = BSL.unpack $ BSL.tail bs
    pn = fromIntegral $ (bytesToWord $ take 2 rs :: Word16)
    fi = bytesToWord $ take 4 $ drop 2 rs
    [ha15, ha14, ha13, ha12, ha11, ha10, ha9, ha8, ha7, ha6, ha5, ha4, ha3, ha2, ha1, ha0] = take 16 $ drop 6 rs
    ha = (bytesToWord [ha15, ha14, ha13, ha12], bytesToWord [ha11, ha10, ha9, ha8], bytesToWord [ha7, ha6, ha5, ha4], bytesToWord [ha3, ha2, ha1, ha0])
    si = bytesToWord $ drop 22 rs
  toLazyByteString (Sock.SockAddrInet pn ha) = 0 `BSL.cons` BSL.append bsPn bsHa
    where
    bsPn = BSL.pack $ wordToBytes (fromIntegral pn :: Word16)
    bsHa = BSL.pack $ wordToBytes ha
  toLazyByteString (Sock.SockAddrInet6 pn fi (ha3, ha2, ha1, ha0) si) = 1 `BSL.cons` BSL.concat [bsPn, bsFi, bsHa, bsSi]
    where
    bsPn = BSL.pack $ wordToBytes (fromIntegral pn :: Word16)
    bsFi = BSL.pack $ wordToBytes fi
    bsHa = BSL.pack $ concatMap wordToBytes [ha3, ha2, ha1, ha0]
    bsSi = BSL.pack $ wordToBytes si

-- to not use BS builder because of two way requirement
wordToBytes :: (FiniteBits a, Integral a) => a -> [Word8]
wordToBytes w = map (\i -> fromIntegral $ w `shiftR` (i * 8)) [byteSize -1 , byteSize - 2 .. 0]
  where byteSize = finiteBitSize w `div` 8
bytesToWord :: forall a. (FiniteBits a, Integral a) => [Word8] -> a
bytesToWord = fst . foldr (\b (r, c) -> (r .|. fromIntegral b `shiftL` c :: a, c + 8)) (0 :: a, 0 :: Int)

newDisplay :: Sock.Socket -> TVar [(Bool, Sock.SockAddr)] -> IO (TVar (Maybe (TChan Frame)))
newDisplay websocket clients = do
  mHPutStrLn stderr "at accept websocket conn"
  (conn, _) <- Sock.accept websocket
  mHPutStrLn stderr "done accepting websocket conn"
  wconnMb <- (WSock.makePendingConnection conn (WSock.ConnectionOptions (return ())) >>= fmap Right . WSock.acceptRequest) `catches` [
              Handler $ \(e :: WSock.HandshakeException) -> return $ Left $ show e
             ,Handler $ \(e :: WSock.ConnectionException) -> return $ Left $ show e]
  case wconnMb of
    Left e -> do
      Sock.close conn
      mHPutStrLn stderr ("handshake exception: " ++ e)
      newTVarIO Nothing
    Right wconn -> do
      chan <- newTChanIO
      mvchan <- newTVarIO $ Just chan
      activeAddress <- atomically $ readTVar clients >>= \cs -> case cs of { [(_, c)] -> newTVar $ Just c; _ -> newTVar Nothing }
      tidrMVar <- newEmptyMVar
      tidsMVar <- newEmptyMVar
      mPutStrLn "got connection"
      sendLock <- newMVar ()
      let wSend msg = withMVar sendLock (\() -> WSock.sendBinaryData wconn (msg :: Message))
      tidUpdater <- forkIO $ forever $ do
        cs <- atomically $ readTVar clients
        wSend $ MCUpdate $ map snd cs
        threadDelay 1000000

      let receiver = handle (\(e :: WSock.ConnectionException) -> mPutStrLn ("received exception in receiver: " ++ show e ++ " -> closing") >> closeMVar tidsMVar) $ do
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
          sender = handle (\(e :: WSock.ConnectionException) -> mPutStrLn ("received exception in sender: " ++ show e ++ " -> closing") >> closeMVar tidrMVar) $ do
            msg <- atomically $ readTChan chan
            sockAddr <- atomically $ readTVar activeAddress
            when (sockAddr == Just (source msg)) $ wSend $ MFrame msg
            sender
          closeMVar mvar = do
            tid <- readMVar mvar
            atomically $ modifyTVar mvchan (\_ -> Nothing)
            killThread tid
            killThread tidUpdater
            Sock.close conn

      tidr <- forkIO receiver
      putMVar tidrMVar tidr
      tids <- forkIO sender
      putMVar tidsMVar tids
      return mvchan

receive :: Sock.Socket -> IO Frame
receive udpSocket = uncurry (flip Frame) `fmap` NBS.recvFrom udpSocket 2048

-- Use System.Timeout
main :: IO ()
main = Sock.withSocketsDo $ bracket (mkSock Sock.Datagram "0.0.0.0" 1337) Sock.close $ \udpSocket -> bracket (mkSock Sock.Stream "0.0.0.0" 8080) Sock.close $ \websocket -> do
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering
  Sock.listen websocket 5
  Sock.setSocketOption websocket Sock.ReuseAddr 1
  Sock.setSocketOption websocket Sock.NoDelay 1
  continue <- newTVarIO True
  Priv.dropUidGid (Left "nobody") (Left "nogroup")
  Priv.status >>= \stat -> mPutStrLn $ "dropped privs to: " ++ show stat
  _ <- Signals.installHandler Signals.sigINT (Signals.CatchOnce $ atomically $ writeTVar continue False) Nothing
  _ <- Signals.installHandler Signals.sigTERM (Signals.CatchOnce $ atomically $ writeTVar continue False) Nothing
  displays <- newTVarIO []
  clients <- newTVarIO []
  forkIO $ forever $ do
    display <- newDisplay websocket clients
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
    {-mPutStrLn "cleaning up"-}
    atomically $ cleanupDisplays displays
    atomically $ cleanupClients clients
  let loop disps' cs' = atomically (readTVar continue) >>= \c -> when c $ do
                disps <- atomically $ readTVar displays
                cs <- atomically $ readTVar clients
                when (disps' /= disps || cs /= cs') $ mPutStrLn $ "number of displays = " ++ show (length disps) ++ "\nnumber of clients = " ++ show (length cs) ++ "\nclients = " ++ show cs
                {-threadDelay 1000000-}
                threadDelay 10000000
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
