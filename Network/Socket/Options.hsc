-- |
-- Module:       Network.Socket.Options
-- Copyright:    (c) Joseph Adams 2012
-- License:      BSD3
-- Maintainer:   joeyadams3.14159@gmail.com
--
-- Documentation is currently lacking.  For now, see @man 7 socket@ and
-- @man 7 tcp@ of the Linux man-pages, or look up setsockopt in MSDN.
{-# LANGUAGE ForeignFunctionInterface #-}
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

#if mingw32_HOST_OS
#include <winsock2.h>
#else
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <sys/socket.h>
#include <sys/types.h>
#endif

import Foreign
import Foreign.C
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
-- Higher-level wrappers

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

------------------------------------------------------------------------
-- Foreign call wrappers

#if mingw32_HOST_OS

foreign import stdcall safe "winsock2.h setsockopt"
    c_setsockopt
        :: #{type SOCKET}   -- ^ SOCKET s
        -> OptLevel         -- ^ int level
        -> OptName          -- ^ int optname
        -> Ptr CChar        -- ^ const char *optval
        -> CInt             -- ^ int optlen
        -> IO CInt

#else

foreign import ccall safe "setsockopt"
    c_setsockopt
        :: CInt                 -- ^ int sockfd
        -> OptLevel             -- ^ int level
        -> OptName              -- ^ int optname
        -> Ptr a                -- ^ const void *optval
        -> #{type socklen_t}    -- ^ socklen_t optlen
        -> IO CInt

#endif

setStorable :: Storable a => OptLevel -> OptName -> Socket -> a -> IO ()
setStorable level name sock x =
    with x $ \ptr ->
        throwSocketErrorIfMinus1_ "setsockopt" $
            let s       = fromIntegral $ fdSocket sock
                optval  = castPtr ptr
                optlen  = fromIntegral $ sizeOf x
             in c_setsockopt s level name optval optlen
