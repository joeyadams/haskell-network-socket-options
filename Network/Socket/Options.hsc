------------------------------------------------------------------------
-- |
-- Module:       Network.Socket.Options
-- Copyright:    (c) Joseph Adams 2012
-- License:      BSD3
-- Maintainer:   joeyadams3.14159@gmail.com
--
-- Documentation is currently lacking.  For now, see @man 7 socket@ and
-- @man 7 tcp@ of the Linux man-pages, or look up setsockopt in MSDN.
{-# LANGUAGE CPP, ForeignFunctionInterface #-}
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
    HasSocket(..),
    Seconds,
    Microseconds,

    -- * Setting socket timeouts
    -- $timeouts
    setSocketTimeouts,
##ifdef __GLASGOW_HASKELL__
    setHandleTimeouts,
##endif
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
import Foreign.C.Types
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peek)
import Network.Socket (Socket, SocketType(..))
##if MIN_VERSION_network(3,1,0)
import Network.Socket (withFdSocket)
##else
import Network.Socket (fdSocket)
##endif
import Network.Socket.Internal (throwSocketErrorIfMinus1_)
import System.Posix.Types (Fd(Fd))

##ifdef __GLASGOW_HASKELL__
import qualified GHC.IO.FD as FD
import System.IO (Handle)

##if mingw32_HOST_OS
import Data.Typeable (cast)
import GHC.IO.Handle.Internals (withHandle_)
import GHC.IO.Handle.Types (Handle__(Handle__, haDevice))
##endif
##endif

-- | The getters and setters in this module can be used not only on 'Socket's,
-- but on raw 'Fd's (file descriptors) as well.
class HasSocket a where
    withFdSocket_ :: a -> (CInt -> IO b) -> IO b

instance HasSocket Fd where
    withFdSocket_ (Fd n) action = action n

instance HasSocket Socket where
##if MIN_VERSION_network(3,1,0)
    withFdSocket_ = withFdSocket
##else
##if MIN_VERSION_network(3,0,0)
    withFdSocket_ sock action = do
        fd <- fdSocket sock
        action fd
##else
    withFdSocket_ sock action = action (fdSocket sock)
##endif
##endif

##ifdef __GLASGOW_HASKELL__
instance HasSocket FD.FD where
    withFdSocket_ s action = action (FD.fdFD s)
##endif

type Seconds        = Int
type Microseconds   = Int64

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
getAcceptConn :: HasSocket sock => sock -> IO Bool
getAcceptConn = getBool #{const SOL_SOCKET} #{const SO_ACCEPTCONN}

getBroadcast :: HasSocket sock => sock -> IO Bool
getBroadcast = getBool #{const SOL_SOCKET} #{const SO_BROADCAST}

getDebug :: HasSocket sock => sock -> IO Bool
getDebug = getBool #{const SOL_SOCKET} #{const SO_DEBUG}

getDontRoute :: HasSocket sock => sock -> IO Bool
getDontRoute = getBool #{const SOL_SOCKET} #{const SO_DONTROUTE}

-- | This option is get-only.
getError :: HasSocket sock => sock -> IO Int
getError = getInt #{const SOL_SOCKET} #{const SO_ERROR}

getKeepAlive :: HasSocket sock => sock -> IO Bool
getKeepAlive = getBool #{const SOL_SOCKET} #{const SO_KEEPALIVE}

getLinger :: HasSocket sock => sock -> IO (Maybe Seconds)
getLinger sock =
    alloca $ \l_onoff_ptr ->
    alloca $ \l_linger_ptr -> do
        throwSocketErrorIfMinus1_ "getsockopt" $
            withFdSocket_ sock $ \sockFd ->
                c_getsockopt_linger sockFd l_onoff_ptr l_linger_ptr
        onoff <- peek l_onoff_ptr
        if onoff /= 0
            then (Just . fromIntegral) `fmap` peek l_linger_ptr
            else return Nothing

getOOBInline :: HasSocket sock => sock -> IO Bool
getOOBInline = getBool #{const SOL_SOCKET} #{const SO_OOBINLINE}

getRecvBuf :: HasSocket sock => sock -> IO Int
getRecvBuf = getInt #{const SOL_SOCKET} #{const SO_RCVBUF}

getRecvTimeout :: HasSocket sock => sock -> IO Microseconds
getRecvTimeout = getTime #{const SOL_SOCKET} #{const SO_RCVTIMEO}

getReuseAddr :: HasSocket sock => sock -> IO Bool
getReuseAddr = getBool #{const SOL_SOCKET} #{const SO_REUSEADDR}

getSendBuf :: HasSocket sock => sock -> IO Int
getSendBuf = getInt #{const SOL_SOCKET} #{const SO_SNDBUF}

getSendTimeout :: HasSocket sock => sock -> IO Microseconds
getSendTimeout = getTime #{const SOL_SOCKET} #{const SO_SNDTIMEO}

-- | This option is get-only.
getType :: HasSocket sock => sock -> IO SocketType
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

getTcpNoDelay :: HasSocket sock => sock -> IO Bool
getTcpNoDelay = getBool #{const IPPROTO_TCP} #{const TCP_NODELAY}

------------------------------------------------------------------------
-- Setting options

setBroadcast :: HasSocket sock => sock -> Bool -> IO ()
setBroadcast = setBool #{const SOL_SOCKET} #{const SO_BROADCAST}

setDebug :: HasSocket sock => sock -> Bool -> IO ()
setDebug = setBool #{const SOL_SOCKET} #{const SO_DEBUG}

setDontRoute :: HasSocket sock => sock -> Bool -> IO ()
setDontRoute = setBool #{const SOL_SOCKET} #{const SO_DONTROUTE}

setKeepAlive :: HasSocket sock => sock -> Bool -> IO ()
setKeepAlive = setBool #{const SOL_SOCKET} #{const SO_KEEPALIVE}

-- | On Windows, the 'Seconds' value is truncated to 16 bits.  This means if a
-- linger time of more than 65535 seconds (about 18.2 hours) is given, it will
-- wrap around.
setLinger :: HasSocket sock => sock -> Maybe Seconds -> IO ()
setLinger sock (Just linger) =
    throwSocketErrorIfMinus1_ "setsockopt" $
        withFdSocket_ sock $ \sockFd ->
            c_setsockopt_linger sockFd 1 (fromIntegral linger)
setLinger sock Nothing =
    throwSocketErrorIfMinus1_ "setsockopt" $
        withFdSocket_ sock $ \sockFd ->
            c_setsockopt_linger sockFd 0 0

setOOBInline :: HasSocket sock => sock -> Bool -> IO ()
setOOBInline = setBool #{const SOL_SOCKET} #{const SO_OOBINLINE}

setRecvBuf :: HasSocket sock => sock -> Int -> IO ()
setRecvBuf = setInt #{const SOL_SOCKET} #{const SO_RCVBUF}

-- | Note the following about timeout values:
--
--  * A value of 0 or less means the operation will never time out
--
--  * On Windows, the timeout is truncated to milliseconds, 32-bit.  However,
--    if the number of microseconds is from 1 to 999, it will be rounded up to
--    one millisecond, to prevent it from being treated as \"never time out\".
setRecvTimeout :: HasSocket sock => sock -> Microseconds -> IO ()
setRecvTimeout = setTime #{const SOL_SOCKET} #{const SO_RCVTIMEO}

setReuseAddr :: HasSocket sock => sock -> Bool -> IO ()
setReuseAddr = setBool #{const SOL_SOCKET} #{const SO_REUSEADDR}

setSendBuf :: HasSocket sock => sock -> Int -> IO ()
setSendBuf = setInt #{const SOL_SOCKET} #{const SO_SNDBUF}

setSendTimeout :: HasSocket sock => sock -> Microseconds -> IO ()
setSendTimeout = setTime #{const SOL_SOCKET} #{const SO_SNDTIMEO}

setTcpNoDelay :: HasSocket sock => sock -> Bool -> IO ()
setTcpNoDelay = setBool #{const IPPROTO_TCP} #{const TCP_NODELAY}

------------------------------------------------------------------------
-- Wrappers

type SockFd     = CInt
type Level      = CInt
type OptName    = CInt

getBool :: HasSocket sock => Level -> OptName -> sock -> IO Bool
getBool level optname sock =
    (/= 0) `fmap` getCInt level optname sock

setBool :: HasSocket sock => Level -> OptName -> sock -> Bool -> IO ()
setBool level optname sock b =
    setCInt level optname sock (fromIntegral $ fromEnum b)

getInt :: HasSocket sock => Level -> OptName -> sock -> IO Int
getInt level optname sock =
    fromIntegral `fmap` getCInt level optname sock

setInt :: HasSocket sock => Level -> OptName -> sock -> Int -> IO ()
setInt level optname sock n =
    setCInt level optname sock (fromIntegral n)

getCInt :: HasSocket sock => Level -> OptName -> sock -> IO CInt
getCInt level optname sock =
    alloca $ \ptr -> do
        throwSocketErrorIfMinus1_ "getsockopt" $
            withFdSocket_ sock $ \sockFd ->
                c_getsockopt_int sockFd level optname ptr
        peek ptr

setCInt :: HasSocket sock => Level -> OptName -> sock -> CInt -> IO ()
setCInt level optname sock n =
    throwSocketErrorIfMinus1_ "setsockopt" $
        withFdSocket_ sock $ \sockFd ->
            c_setsockopt_int sockFd level optname n

getTime :: HasSocket sock => Level -> OptName -> sock -> IO Microseconds
getTime level optname sock =
    alloca $ \ptr -> do
        throwSocketErrorIfMinus1_ "getsockopt" $
            withFdSocket_ sock $ \sockFd ->
                c_getsockopt_time sockFd level optname ptr
        peek ptr

setTime :: HasSocket sock => Level -> OptName -> sock -> Microseconds -> IO ()
setTime level optname sock usec =
    throwSocketErrorIfMinus1_ "setsockopt" $
        withFdSocket_ sock $ \sockFd ->
            c_setsockopt_time sockFd level optname usec

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
-- Setting socket timeouts

{- $timeouts

The following functions are provided to work around a problem with network IO
on Windows.  They are no-ops on other systems.  Use them /in addition to/, not
/instead of/, asynchronous exceptions (e.g. "System.Timeout") to time out
network operations.

The problem is that GHC currently does not have proper IO manager support for
Windows.  On Unix, GHC uses non-blocking IO and @select@\/@epoll@\/@kqueue@ for
efficient multiplexing.  On Windows, it uses blocking FFI (foreign function
interface) calls.  An FFI call blocks the OS thread it is called from, and
cannot be interrupted by an asynchronous exception.  This means that if a send
or receive operation hangs indefinitely, the thread hangs indefinitely, and
cannot be killed.  Thus, the following timeout will not work on Windows, in a
program compiled with @-threaded@:

@
'System.Timeout.timeout' 120000000 $ 'Network.Socket.recv' sock len
@

In a program compiled without @-threaded@, the timeout will work, but it will
leak an OS thread until data arrives on the socket.

We can work around the problem by performing the IO in another thread:

>wrappedRecv :: Socket -> Int -> IO String
>wrappedRecv sock len = do
>    mv <- newEmptyMVar
>    bracket (forkIO $ recv sock len >>= putMVar mv)
>            (forkIO . killThread)
>               -- Call 'killThread' in a forked thread, as it will block until
>               -- the exception has been delivered to the target thread.
>            (\_ -> takeMVar mv)

This will behave correctly, but it will leak an OS thread if
'Network.Socket.recv' hangs indefinitely.  If about 1000 OS threads are hung on
'Network.Socket.recv' calls, the program will run out of address space and
crash (assuming 32-bit Windows, with default settings).

Socket timeouts can be used to work around the problem.  In a program compiled
for Windows with @-threaded@, when a receive or send operation times out, it
will fail with an exception, and will not leak an OS thread.  Without
@-threaded@, it will leak an OS thread, unfortunately.

Socket timeouts have no effect on 'Network.Socket.connect', which does seem to
time out on its own at some point.  They also have no effect for
'System.IO.hWaitForInput' when an explicit timeout is given.

-}

-- | On Windows, set the socket's @SO_RCVTIMEO@ and @SO_SNDTIMEO@ values to the
-- ones given.  On other platforms, do nothing.
setSocketTimeouts
    :: HasSocket sock
    => sock
    -> Microseconds -- ^ Receive timeout
    -> Microseconds -- ^ Send timeout
    -> IO ()
##if mingw32_HOST_OS
setSocketTimeouts sock recv_usec send_usec = do
    setRecvTimeout sock recv_usec
    setSendTimeout sock send_usec
##else
setSocketTimeouts _ _ _ = return ()
##endif


##ifdef __GLASGOW_HASKELL__

-- | On Windows, set timeouts for a socket that has already been wrapped in a
-- 'Handle' by 'Network.connectTo' or 'Network.accept'.  On other platforms, do
-- nothing.
setHandleTimeouts
    :: Handle
    -> Microseconds -- ^ Receive timeout
    -> Microseconds -- ^ Send timeout
    -> IO ()
##if mingw32_HOST_OS
setHandleTimeouts h recv_usec send_usec =
    withHandle_ "setHandleTimeouts" h $ \Handle__{haDevice = dev} ->
        case cast dev of
            Just fd | FD.fdIsSocket_ fd /= 0 -> do
                setRecvTimeout fd recv_usec
                setSendTimeout fd send_usec
            _ -> return ()
##else
setHandleTimeouts _ _ _ = return ()
##endif

##endif
