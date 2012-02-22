{-
When running this on Windows, watch the Handles and Threads counters in
Windows Task Manager.  These are hidden by default, but can be displayed if you
go to View -> Select Columns.
-}
import Prelude hiding (log)
import Log

import Control.Concurrent
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Network
import Network.Socket.Options (setHandleTimeouts)
import System.IO

class MonadFork m where
    forkM :: m () -> m ThreadId

instance MonadFork IO where
    forkM = forkIO

instance MonadFork m => MonadFork (LogT m) where
    forkM (LogT (ReaderT m)) = LogT (ReaderT (forkM . m))

server :: Socket -> IO ()
server sock = withLogContext "server" $
    let loop n = do
            (h, host, port) <- liftIO $ accept sock
            _ <- forkM $ withLogContext (show n) $ do
                log $ "Received connection from " ++ host ++ ":" ++ show port
                liftIO $ hSetBuffering h LineBuffering
                log "Sending \"one\""
                liftIO $ hPutStrLn h "one"
                log "Waiting 30 seconds"
                liftIO $ threadDelay 30000000
                log "Sending \"two\""
                liftIO $ hPutStrLn h "two"
                log "Closing the handle"
                liftIO $ hClose h
                log "Done"
            loop $! n+1
     in loop (1 :: Integer)

client :: Integer -> IO ()
client n = withLogContext "client" $
           withLogContext (show n) $ do
    log "Connecting to localhost:1234"
    h <- liftIO $ connectTo "localhost" $ PortNumber 1234
    liftIO $ setHandleTimeouts h 2000000 2000000

    log "Connected.  Getting first line"
    line1 <- liftIO $ hGetLine h
    log $ "First line: " ++ line1

    recv_tid <- forkM $ do
        log "Getting second line"
        line2 <- liftIO $ hGetLine h
        log $ "Second line: " ++ line2

    liftIO $ threadDelay 1000000
    log "Killing receiving thread"
    liftIO $ killThread recv_tid
    log "Done killing"

main :: IO ()
main = do
    hSetBuffering stderr LineBuffering

    sock <- liftIO $ listenOn $ PortNumber 1234
    log "Listening on port 1234"

    _ <- forkIO $ server sock
    mapM_ client [1..]
