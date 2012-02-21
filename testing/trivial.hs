-- Make sure getting and setting work.  This doesn't test if the options
-- actually do anything, it just makes sure getsockopt reflects changes by
-- setsockopt.
--
-- TODO: Add test for getError

import qualified Network.Socket.Options as Opt

import Prelude hiding (catch, last)

import Control.Exception
import Control.Monad
import Network.Socket

type Getter a = Socket -> IO a
type Setter a = Socket -> a -> IO ()

test_getType :: IO ()
test_getType = runTest "getType" $ do
    f AF_INET Stream    0
    f AF_INET Datagram  0
    f AF_INET Raw       1
    where
        f domain type_ protocol = do
            e <- try $ socket domain type_ protocol
            case e of
                Left ex ->
                    not_ok (show type_) $ show (ex :: SomeException)
                Right sock -> do
                    t <- Opt.getType sock
                    if t == type_
                        then ok     (show type_)
                        else not_ok (show type_) $ "getType returned " ++ show t ++ " instead"

test_getAcceptConn :: IO ()
test_getAcceptConn = runTest "getAcceptConn" $ do
    sock <- socket AF_INET Stream defaultProtocol
    a1 <- Opt.getAcceptConn sock
    expect (a1 == False) "getAcceptConn returns False for a fresh socket"

    bindSocket sock $ SockAddrInet 1234 iNADDR_ANY
    a2 <- Opt.getAcceptConn sock
    expect (a2 == False) "getAcceptConn returns False after bindSocket"

    listen sock 5
    a3 <- Opt.getAcceptConn sock
    expect (a3 == True) "getAcceptConn returns True after listen is called"

testBool :: String -> Getter Bool -> Setter Bool -> IO ()
testBool name get set = runTest name $ do
    sock <- socket AF_INET Stream defaultProtocol

    v1 <- get sock
    ok $ "Initial value: " ++ show v1

    set sock (not v1)
    v2 <- get sock
    expect (v2 == not v1) "setsockopt changed result of getsockopt"

    set sock v1
    v3 <- get sock
    expect (v3 == v1) "setsockopt changed result of getsockopt again"

testInt :: String -> Getter Int -> Setter Int -> IO ()
testInt name get set = runTest name $ do
    sock <- socket AF_INET Stream defaultProtocol

    v1 <- get sock
    ok $ "Initial value: " ++ show v1

    let setTo last v = do
            set sock v
            actual <- get sock
            if actual /= last
                then ok     ("set " ++ show v ++ ", got " ++ show actual)
                else not_ok (show v) ("value did not change")
            return actual

    _ <- foldM setTo v1 [0, 10000, 1, 1000000, 0]
    return ()

testMicroseconds
    :: String
    -> Getter Opt.Microseconds
    -> Setter Opt.Microseconds
    -> IO ()
testMicroseconds name get set = runTest name $ do
    sock <- socket AF_INET Stream defaultProtocol

    v1 <- get sock
    ok $ "Initial value: " ++ show v1
    
    let setTo (setval, getval) = do
            set sock setval
            actual <- get sock
            if actual == getval
                then ok     ("set " ++ show setval ++ ", get " ++ show getval)
                else not_ok ("set " ++ show setval ++ ", get " ++ show getval)
                            ("actually got " ++ show actual)

    mapM_ setTo
        [ (0,0)
        , (10000, 10000)
        , (4000000000000, 4000000000000)
        , (1, 1)
        , (0,0)
        ]

test_linger :: IO ()
test_linger = runTest "SO_LINGER" $ do
    sock <- socket AF_INET Stream defaultProtocol

    linger <- Opt.getLinger sock
    ok $ "Initial value: " ++ show linger

    let v1 = Nothing
        v2 = Just 1
        v3 = Just 30000

        setTo v = do
            Opt.setLinger sock v
            actual <- Opt.getLinger sock
            if actual == v
                then ok     (show v)
                else not_ok (show v) ("getLinger returned " ++ show actual ++ " instead")

    mapM_ setTo [v1, v2, v3, v1]

runTest :: String -> IO () -> IO ()
runTest name action = do
    putStrLn $ name ++ ":"
    action `catch` \ex -> do
        putStrLn ""
        putStrLn $ "Test " ++ name ++ " encountered an exception:\n    "
                ++ show (ex :: SomeException)
    putStrLn ""

ok :: String -> IO ()
ok msg = putStrLn $ "    ok:     " ++ msg

not_ok :: String -> String -> IO ()
not_ok msg reason = do
    putStrLn $ "    not ok: " ++ msg
    putStrLn $ "        reason: " ++ reason

expect :: Bool -> String -> IO ()
expect b msg =
    if b
        then putStrLn $ "    ok:     " ++ msg
        else putStrLn $ "    not ok: " ++ msg

main :: IO ()
main = do
    test_getType
    test_getAcceptConn

    testBool "SO_BROADCAST" Opt.getBroadcast    Opt.setBroadcast
    testBool "SO_DEBUG"     Opt.getDebug        Opt.setDebug
    testBool "SO_DONTROUTE" Opt.getDontRoute    Opt.setDontRoute
    testBool "SO_KEEPALIVE" Opt.getKeepAlive    Opt.setKeepAlive
    testBool "SO_OOBINLINE" Opt.getOOBInline    Opt.setOOBInline
    testBool "SO_REUSEADDR" Opt.getReuseAddr    Opt.setReuseAddr
    testBool "TCP_NODELAY"  Opt.getTcpNoDelay   Opt.setTcpNoDelay

    testInt  "SO_RCVBUF"    Opt.getRecvBuf      Opt.setRecvBuf
    testInt  "SO_SNDBUF"    Opt.getSendBuf      Opt.setSendBuf

    testMicroseconds "SO_RCVTIMEO" Opt.getRecvTimeout Opt.setRecvTimeout
    testMicroseconds "SO_SNDTIMEO" Opt.getSendTimeout Opt.setSendTimeout

    test_linger

    putStrLn "Tests finished."
