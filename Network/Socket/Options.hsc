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
    -- * Getting options
    getAcceptConn,
    getBroadcast,
    getDebug,
    getDontRoute,
    getError,
    getKeepAlive,
    getLinger,
    getOOBInline,
    getRecvBuf,
    getRecvTimeout,
    getReuseAddr,
    getSendBuf,
    getSendTimeout,
    getType,

    -- ** TCP
    getTcpNoDelay,

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
import Foreign.C.Error (throwErrnoIfMinus1_)
import Foreign.C.Types
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peek)
import Network.Socket (Socket, SocketType(..), fdSocket)

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

------------------------------------------------------------------------
-- Getting options

-- | This option is get-only.
getAcceptConn :: Socket -> IO Bool
getAcceptConn = getBool #{const SOL_SOCKET} #{const SO_ACCEPTCONN}

getBroadcast :: Socket -> IO Bool
getBroadcast = getBool #{const SOL_SOCKET} #{const SO_BROADCAST}

getDebug :: Socket -> IO Bool
getDebug = getBool #{const SOL_SOCKET} #{const SO_DEBUG}

getDontRoute :: Socket -> IO Bool
getDontRoute = getBool #{const SOL_SOCKET} #{const SO_DONTROUTE}

-- | This option is get-only.
getError :: Socket -> IO Int
getError = getInt #{const SOL_SOCKET} #{const SO_ERROR}

getKeepAlive :: Socket -> IO Bool
getKeepAlive = getBool #{const SOL_SOCKET} #{const SO_KEEPALIVE}

getLinger :: Socket -> IO Linger
getLinger sock =
    alloca $ \l_onoff_ptr ->
    alloca $ \l_linger_ptr -> do
        throwSocketErrorIfMinus1_ "getsockopt" $
            c_getsockopt_linger (fdSocket sock) l_onoff_ptr l_linger_ptr
        onoff  <- (/= 0)       `fmap` peek l_onoff_ptr
        linger <- fromIntegral `fmap` peek l_linger_ptr
        return Linger{ l_onoff = onoff, l_linger = linger }

getOOBInline :: Socket -> IO Bool
getOOBInline = getBool #{const SOL_SOCKET} #{const SO_OOBINLINE}

getRecvBuf :: Socket -> IO Int
getRecvBuf = getInt #{const SOL_SOCKET} #{const SO_RCVBUF}

getRecvTimeout :: Socket -> IO Microseconds
getRecvTimeout = getTime #{const SOL_SOCKET} #{const SO_RCVTIMEO}

getReuseAddr :: Socket -> IO Bool
getReuseAddr = getBool #{const SOL_SOCKET} #{const SO_REUSEADDR}

getSendBuf :: Socket -> IO Int
getSendBuf = getInt #{const SOL_SOCKET} #{const SO_SNDBUF}

getSendTimeout :: Socket -> IO Microseconds
getSendTimeout = getTime #{const SOL_SOCKET} #{const SO_SNDTIMEO}

-- | This option is get-only.
getType :: Socket -> IO SocketType
getType sock =
    toSocketType `fmap` getCInt #{const SOL_SOCKET} #{const SO_TYPE} sock

toSocketType :: CInt -> SocketType
toSocketType t = case t of
#ifdef SOCK_STREAM
    #{const SOCK_STREAM} -> Stream
#endif
#ifdef SOCK_DGRAM
    #{const SOCK_DGRAM} -> Datagram
#endif
#ifdef SOCK_RAW
    #{const SOCK_RAW} -> Raw
#endif
#ifdef SOCK_RDM
    #{const SOCK_RDM} -> RDM
#endif
#ifdef SOCK_SEQPACKET
    #{const SOCK_SEQPACKET} -> SeqPacket
#endif
    _ -> error $ "Network.Socket.Options.getType: Unknown socket type #" ++ show t

getTcpNoDelay :: Socket -> IO Bool
getTcpNoDelay = getBool #{const IPPROTO_TCP} #{const TCP_NODELAY}

------------------------------------------------------------------------
-- Setting options

setBroadcast :: Socket -> Bool -> IO ()
setBroadcast = setBool #{const SOL_SOCKET} #{const SO_BROADCAST}

setDebug :: Socket -> Bool -> IO ()
setDebug = setBool #{const SOL_SOCKET} #{const SO_DEBUG}

setDontRoute :: Socket -> Bool -> IO ()
setDontRoute = setBool #{const SOL_SOCKET} #{const SO_DONTROUTE}

setKeepAlive :: Socket -> Bool -> IO ()
setKeepAlive = setBool #{const SOL_SOCKET} #{const SO_KEEPALIVE}

setLinger :: Socket -> Linger -> IO ()
setLinger sock l =
    throwSocketErrorIfMinus1_ "setsockopt" $
        c_setsockopt_linger (fdSocket sock)
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

getBool :: Level -> OptName -> Socket -> IO Bool
getBool level optname sock =
    (/= 0) `fmap` getCInt level optname sock

setBool :: Level -> OptName -> Socket -> Bool -> IO ()
setBool level optname sock b =
    setCInt level optname sock (fromIntegral $ fromEnum b)

getInt :: Level -> OptName -> Socket -> IO Int
getInt level optname sock =
    fromIntegral `fmap` getCInt level optname sock

setInt :: Level -> OptName -> Socket -> Int -> IO ()
setInt level optname sock n =
    setCInt level optname sock (fromIntegral n)

getCInt :: Level -> OptName -> Socket -> IO CInt
getCInt level optname sock =
    alloca $ \ptr -> do
        throwSocketErrorIfMinus1_ "getsockopt" $
            c_getsockopt_int (fdSocket sock) level optname ptr
        peek ptr

setCInt :: Level -> OptName -> Socket -> CInt -> IO ()
setCInt level optname sock n =
    throwSocketErrorIfMinus1_ "setsockopt" $
        c_setsockopt_int (fdSocket sock) level optname n

getTime :: Level -> OptName -> Socket -> IO Microseconds
getTime level optname sock =
    alloca $ \ptr -> do
        throwSocketErrorIfMinus1_ "getsockopt" $
            c_getsockopt_time (fdSocket sock) level optname ptr
        peek ptr

setTime :: Level -> OptName -> Socket -> Microseconds -> IO ()
setTime level optname sock usec =
    throwSocketErrorIfMinus1_ "setsockopt" $
        c_setsockopt_time (fdSocket sock) level optname usec

foreign import ccall
    c_getsockopt_int :: SockFd -> Level -> OptName -> Ptr CInt -> IO CInt

foreign import ccall
    c_setsockopt_int :: SockFd -> Level -> OptName -> CInt -> IO CInt

foreign import ccall
    c_getsockopt_time :: SockFd -> Level -> OptName -> Ptr Int64 -> IO CInt

foreign import ccall
    c_setsockopt_time :: SockFd -> Level -> OptName -> Int64 -> IO CInt

foreign import ccall
    c_getsockopt_linger :: SockFd
                        -> Ptr CInt -- ^ l_onoff
                        -> Ptr CInt -- ^ l_linger
                        -> IO CInt

foreign import ccall
    c_setsockopt_linger :: SockFd
                        -> CInt     -- ^ l_onoff
                        -> CInt     -- ^ l_linger
                        -> IO CInt

------------------------------------------------------------------------
-- Error handling

-- TODO: Use WSAGetLastError and FormatMessage on Windows, to avoid error
--       messages that say "no error".  The network package should do the same.
throwSocketErrorIfMinus1_
    :: String
    -> IO CInt
    -> IO ()
throwSocketErrorIfMinus1_ = throwErrnoIfMinus1_
