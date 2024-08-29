-- | Future values (values that can only be set once)
module Haskus.Utils.STM.Future
   ( Future
   , FutureSource
   , newFuture
   , newFutureIO
   , waitFuture
   , pollFuture
   , pollFutureIO
   , setFuture
   , setFutureIO
   , setFuture'
   )
where

import Haskus.Utils.STM
import Haskus.Utils.Flow

-- | Future value of type a
newtype Future a       = Future (TMVar a)

-- | Setter for a future value
newtype FutureSource a = FutureSource (TMVar a)

-- | Create a Future and its source
newFuture :: STM (Future a, FutureSource a)
newFuture = do
   m <- newEmptyTMVar
   return (Future m, FutureSource m)

-- | `newFuture` in `IO`
newFutureIO :: MonadIO m => m (Future a, FutureSource a)
newFutureIO = do
   m <- liftIO newEmptyTMVarIO
   return (Future m, FutureSource m)

-- | Set a future
setFuture :: a -> FutureSource a -> STM ()
setFuture a m = void (setFuture' a m)

-- | Set a future in IO
setFutureIO :: MonadIO m => a -> FutureSource a -> m ()
setFutureIO a m = atomically (setFuture a m)

-- | Set a future
--
-- Return False if it has already been set
setFuture' :: a -> FutureSource a -> STM Bool
setFuture' a (FutureSource m) = tryPutTMVar m a

-- | Wait for a future
waitFuture :: Future a -> STM a
waitFuture (Future m) = readTMVar m

-- | Poll a future
pollFuture :: Future a -> STM (Maybe a)
pollFuture (Future m) = tryReadTMVar m

-- | `pollFuture` in `IO`
pollFutureIO :: MonadIO m => Future a -> m (Maybe a)
pollFutureIO = atomically . pollFuture
