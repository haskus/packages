-- | STM helpers
module Haskus.Utils.STM
   ( S.STM
   , S.retry
   , atomically
   -- ** TVar
   , S.TVar
   , newTVarIO
   , readTVarIO
   , S.writeTVar
   , writeTVarIO
   , S.readTVar
   , S.newTVar
   , S.swapTVar
   , swapTVarIO
   , S.modifyTVar
   , S.modifyTVar'
   -- ** TMVar
   , S.TMVar
   , newTMVarIO
   , S.isEmptyTMVar
   , S.newEmptyTMVar
   , S.newEmptyTMVarIO
   , S.readTMVar
   , S.takeTMVar
   , S.putTMVar
   , S.swapTMVar
   , S.tryReadTMVar
   , S.tryPutTMVar
   , S.tryTakeTMVar
   -- ** TChan
   , S.TChan
   , newBroadcastTChanIO
   , S.newBroadcastTChan
   , S.writeTChan
   , S.dupTChan
   , S.cloneTChan
   , S.readTChan
   )
where

import Control.Concurrent.STM (STM, TVar, TMVar, TChan)
import qualified Control.Concurrent.STM as S
import Haskus.Utils.Monad

-- | Execute an STM transaction atomically
atomically :: MonadIO m => STM a -> m a
atomically = liftIO . S.atomically

-- | Read a TVar in an IO monad
readTVarIO :: MonadIO m => TVar a -> m a
readTVarIO = liftIO . S.readTVarIO

-- | Write a TVar in an IO monad
writeTVarIO :: MonadIO m => TVar a -> a -> m ()
writeTVarIO v a = atomically (S.writeTVar v a)

-- | Swap a TVar in an IO monad
swapTVarIO :: MonadIO m => TVar a -> a -> m a
swapTVarIO v a = atomically (S.swapTVar v a)

-- | Create a broadcast channel
newBroadcastTChanIO :: MonadIO m => m (TChan a)
newBroadcastTChanIO = liftIO S.newBroadcastTChanIO

-- | Create a TVar
newTVarIO :: MonadIO m => a -> m (TVar a)
newTVarIO = liftIO . S.newTVarIO

-- | Create a TMVar
newTMVarIO :: MonadIO m => a -> m (TMVar a)
newTMVarIO = liftIO . S.newTMVarIO
