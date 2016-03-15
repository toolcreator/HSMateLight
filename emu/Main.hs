import Network.WebSockets
import qualified Network.Socket as Sock
import qualified Network.Socket.ByteString.Lazy as NBSL
import qualified Data.ByteString.Lazy as BSL
import Control.Monad

handler :: Sock.Socket -> ServerApp
handler sock pending = do
  putStrLn "got connection"
  conn <- acceptRequest pending
  {-forkPingThread conn 30-}
  forever $ do
    bs <- NBSL.recv sock 4096
    sendBinaryData conn bs

-- use bracket for close...
main :: IO ()
main = Sock.withSocketsDo $ do
  socket <- Sock.socket Sock.AF_INET Sock.Datagram Sock.defaultProtocol
  addr <- Sock.inet_addr "127.0.0.1"
  Sock.bind socket (Sock.SockAddrInet (fromIntegral 1337) addr)
  websocket <- makeListenSocket "127.0.0.1" 8080
  (conn, _) <- Sock.accept websocket
  pending <- makePendingConnection conn (ConnectionOptions (return ()))
  handler socket pending
  Sock.close conn
  Sock.close websocket
  Sock.close socket
