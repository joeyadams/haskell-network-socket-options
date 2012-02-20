-- |
-- Module:       Network.Socket.Options
-- Copyright:    (c) Joseph Adams 2012
-- License:      BSD3
-- Maintainer:   joeyadams3.14159@gmail.com
--
-- Documentation is currently lacking.  For now, see @man 7 socket@ and
-- @man 7 tcp@ of the Linux man-pages, or look up setsockopt in MSDN.
{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS -fno-warn-unused-imports #-}
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
    Microseconds,
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

import Data.Int (Int64)
import Foreign
import Foreign.C
import Network.Socket

type Seconds        = Int
type Microseconds   = Int64

data Linger
    = Linger
        { l_onoff   :: !Bool
        , l_linger  :: !Seconds
        }

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
setLinger sock l = c_setsockopt_linger (fdSocket sock)
                                       (fromIntegral $ fromEnum $ l_onoff l)
                                       (fromIntegral $ l_linger l)

setOOBInline :: Socket -> Bool -> IO ()
setOOBInline = setBool #{const SOL_SOCKET} #{const SO_OOBINLINE}

setRecvBuf :: Socket -> Int -> IO ()
setRecvBuf = setInt #{const SOL_SOCKET} #{const SO_RCVBUF}

-- | Note the following about timeout values:
--
--  * A value of 0 or less means the operation will never time out
--
--  * On Windows, the timeout is truncated to milliseconds, 32-bit.  However,
--    if the number of microseconds is from 1 to 999, it will be rounded up to
--    one millisecond, to prevent it from being treated as "never time out".
setRecvTimeout :: Socket -> Microseconds -> IO ()
setRecvTimeout = setTime #{const SOL_SOCKET} #{const SO_RCVTIMEO}

setReuseAddr :: Socket -> Bool -> IO ()
setReuseAddr = setBool #{const SOL_SOCKET} #{const SO_REUSEADDR}

setSendBuf :: Socket -> Int -> IO ()
setSendBuf = setInt #{const SOL_SOCKET} #{const SO_SNDBUF}

setSendTimeout :: Socket -> Microseconds -> IO ()
setSendTimeout = setTime #{const SOL_SOCKET} #{const SO_SNDTIMEO}

setTcpNoDelay :: Socket -> Bool -> IO ()
setTcpNoDelay = setBool #{const IPPROTO_TCP} #{const TCP_NODELAY}

------------------------------------------------------------------------
-- Wrappers

type SockFd     = CInt
type Level      = CInt
type OptName    = CInt

setBool :: Level -> OptName -> Socket -> Bool -> IO ()
setBool level optname sock b =
    c_setsockopt_int (fdSocket sock) level optname (fromIntegral $ fromEnum b)

setInt :: Level -> OptName -> Socket -> Int -> IO ()
setInt level optname sock n =
    c_setsockopt_int (fdSocket sock) level optname (fromIntegral n)

setTime :: Level -> OptName -> Socket -> Microseconds -> IO ()
setTime level optname sock usec =
    c_setsockopt_time (fdSocket sock) level optname usec

foreign import ccall
    c_setsockopt_int :: SockFd -> Level -> OptName -> CInt -> IO ()

foreign import ccall
    c_setsockopt_time :: SockFd -> Level -> OptName -> Int64 -> IO ()

foreign import ccall
    c_setsockopt_linger :: SockFd
                        -> CInt     -- ^ l_onoff
                        -> CInt     -- ^ l_linger
                        -> IO ()
