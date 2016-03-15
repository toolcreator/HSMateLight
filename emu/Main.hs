import Network.WebSockets
import qualified Network.Socket as Sock
import qualified Network.Socket.ByteString.Lazy as NBSL
import qualified Data.ByteString.Lazy as BSL
import Control.Monad

-- what on connectionc close?
handler :: Sock.Socket -> ServerApp
handler sock pending = do
  putStrLn "got connection"
  conn <- acceptRequest pending
  {-forkPingThread conn 30-}
  forever $ do
    bs <- NBSL.recv sock 4096
    sendBinaryData conn bs

main :: IO ()
main = Sock.withSocketsDo $ do
  socket <- Sock.socket Sock.AF_INET Sock.Datagram Sock.defaultProtocol
  addr <- Sock.inet_addr "127.0.0.1"
  Sock.bind socket (Sock.SockAddrInet (fromIntegral 1337) addr)
  runServer "127.0.0.1" 8080 $ handler socket
  Sock.close socket
