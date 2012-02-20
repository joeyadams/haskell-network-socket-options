module Network.Socket.Options
    (
    -- * Setting options
    setBroadcast,
    setDebug,
    setDontRoute,
    setKeepAlive,
    setLinger,
    setOOBInline,
    setRecvBuf,
    setRecvTimeout,
    setReuseAddr,
    setSendBuf,
    setSendTimeout,

    -- ** TCP
    setTcpNoDelay,

    -- * Getting options


    -- * Types
    Seconds,
    Linger(..),
    ) where

import Network.Socket

data Linger
    = Linger
        { l_onoff   :: Bool
        , l_linger  :: Seconds
        }

type Seconds = Int

instance Storable Linger

{-
It would be cute to have:

    data SocketOption a = ...

so we can say:

    setSocketOption :: Socket -> SocketOption a -> a -> IO ()
    getSocketOption :: Socket -> SocketOption a -> IO a

However, that's probably less convenient to use, and it bars socket options
that support get but not set or vice versa (e.g. SO_ACCEPTCONN and SO_TYPE).
-}

setBroadcast :: Socket -> Bool -> IO ()
setBroadcast = setBool #{const SOL_SOCKET} #{const SO_BROADCAST}

setDebug :: Socket -> Bool -> IO ()
setDebug = setBool #{const SOL_SOCKET} #{const SO_DEBUG}

setDontRoute :: Socket -> Bool -> IO ()
setDontRoute = setBool #{const SOL_SOCKET} #{const SO_DONTROUTE}

setKeepAlive :: Socket -> Bool -> IO ()
setKeepAlive = setBool #{const SOL_SOCKET} #{const SO_KEEPALIVE}

setLinger :: Socket -> Linger -> IO ()
setLinger = setStorable #{const SOL_SOCKET} #{const SO_LINGER}

setOOBInline :: Socket -> Bool -> IO ()
setOOBInline = setBool #{const SOL_SOCKET} #{const SO_OOBINLINE}

setRecvBuf :: Socket -> Int -> IO ()
setRecvBuf = setInt #{const SOL_SOCKET} #{const SO_RCVBUF}

setRecvTimeout :: Socket -> Seconds -> IO ()
setRecvTimeout = setTime #{const SOL_SOCKET} #{const SO_RCVTIMEO}

setReuseAddr :: Socket -> Bool -> IO ()
setReuseAddr = setBool #{const SOL_SOCKET} #{const SO_REUSEADDR}

setSendBuf :: Socket -> Int -> IO ()
setSendBuf = setInt #{const SOL_SOCKET} #{const SO_SNDBUF}

setSendTimeout :: Socket -> Seconds -> IO ()
setSendTimeout = setTime #{const SOL_SOCKET} #{const SO_SNDTIMEO}

setTcpNoDelay :: Socket -> Bool -> IO ()
setTcpNoDelay = setBool #{const IPPROTO_TCP} #{const TCP_NODELAY}

------------------------------------------------------------------------

type OptLevel = CInt
type OptName = CInt

setBool :: OptLevel -> OptName -> Socket -> Bool -> IO ()
setBool = setWith (fromIntegral . fromEnum :: Bool -> CInt)

setInt :: OptLevel -> OptName -> Socket -> Int -> IO ()
setInt = setWith (fromIntegral :: Int -> CInt)

setTime :: OptLevel -> OptName -> Socket -> Seconds -> IO ()
setTime = undefined

setWith :: Storable b => (a -> b) -> OptLevel -> OptName -> Socket -> a -> IO ()
setWith f level name sock = setStorable level name sock . f

setStorable :: Storable a => OptLevel -> OptName -> Socket -> a -> IO ()
setStorable = undefined
