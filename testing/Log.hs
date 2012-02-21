{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Log (
    -- * Basic logging
    logIO,

    -- * Logging with prepended context
    MonadLog(..),
    LogT(..),
    withLogContext,
) where

import Prelude hiding (log)

import Control.Applicative (Applicative)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Reader (ReaderT(ReaderT))
import Data.Time.LocalTime (getZonedTime)
import Data.Time.Format (formatTime)
import System.IO (hPutStrLn, stderr)
import System.Locale (defaultTimeLocale)

-- | Write a message to standard error, prepending the time.
logIO :: String -> IO ()
logIO msg = do
    time <- getZonedTime
    let time_str = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" time
    hPutStrLn stderr $ time_str ++ "  " ++ msg

class Monad m => MonadLog m where
    log :: String -> m ()

instance MonadLog IO where
    log = logIO

newtype LogT m a = LogT (ReaderT (String -> m ()) m a)
    deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans LogT where
    lift = LogT . ReaderT . const

instance Monad m => MonadLog (LogT m) where
    log msg = LogT $ ReaderT $ \f -> f msg

withLogContext :: MonadLog m => String -> LogT m a -> m a
withLogContext ctx (LogT (ReaderT m)) = m (\msg -> log $ ctx ++ ": " ++ msg)
