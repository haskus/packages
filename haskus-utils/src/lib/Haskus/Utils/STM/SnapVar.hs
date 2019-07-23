-- | Snapshotable STM variables
--
-- A `SnapVar` is like a `TVar` except that they have a context which can
-- enable a snapshot mode. When in snapshot mode, the current values are not
-- erased by writes. Instead each SnapVar can have 2 values: the value at the
-- time of the snapshot and its current value.
--
-- There can be only a single snapshot at a time. When the snapshot mode is
-- exited, the variables only keep the current value alive, not the snapshot
-- one.
--
module Haskus.Utils.STM.SnapVar
   ( SnapVar
   , SnapContext
   , newSnapContextIO
   , newSnapContext
   , newSnapVarIO
   , newSnapVar
   , writeSnapVar
   , writeSnapVarIO
   , readSnapVar
   , readSnapVarIO
   , modifySnapVar
   , modifySnapVarIO
   -- * Snapshot
   , withSnapshot
   , readSnapshot
   , readSnapshotIO
   )
where

import Haskus.Utils.STM
import Haskus.Utils.Monad

-- | A snapshot variable
data SnapVar a = SnapVar
   { snapContext   :: !SnapContext      -- ^ Snapshot context
   , snapValue     :: !(TVar a)         -- ^ Snapshot value (during snapshot) or current value (otherwise)
   , snapNextValue :: !(TVar (Maybe a)) -- ^ Next value (during snapshot)
   }

-- | Snapshot context
data SnapContext = SnapContext
   { snapContextState    :: !(TVar SnapState) -- ^ Snapshot state
   , snapContextUpdaters :: !(TVar [STM ()])  -- ^ Variable updaters (on snapshot exit)
   }

-- | Snapshot state
data SnapState
   = NoSnapshot   -- ^ No snapshot active
   | Snapshot     -- ^ Snapshot active
   | SnapshotExit -- ^ Exiting the snapshot


-- | Create a new snapshot context
newSnapContextIO :: MonadIO m => m SnapContext
newSnapContextIO = SnapContext <$> newTVarIO NoSnapshot <*> newTVarIO []

-- | Create a new snapshot context
newSnapContext :: STM SnapContext
newSnapContext = SnapContext <$> newTVar NoSnapshot <*> newTVar []

-- | Create a new SnapVar
newSnapVarIO :: MonadIO m => SnapContext -> a -> m (SnapVar a)
newSnapVarIO ctx v = SnapVar <$> return ctx <*> newTVarIO v <*> newTVarIO Nothing

-- | Create a new SnapVar
newSnapVar :: SnapContext -> a -> STM (SnapVar a)
newSnapVar ctx v = SnapVar <$> return ctx <*> newTVar v <*> newTVar Nothing

-- | Write a SnapVar
writeSnapVar :: SnapVar a -> a -> STM ()
writeSnapVar v a = do
   state <- readTVar (snapContextState (snapContext v))
   case state of
      NoSnapshot   -> writeTVar (snapValue v) a
      SnapshotExit -> do
         writeTVar (snapValue v) a
         -- update next-value too to be sure that no updater will erase our
         -- value and that a future read will be correct
         writeTVar (snapNextValue v) Nothing
      Snapshot     -> do
         -- write into next-value and get old value
         mv <- swapTVar (snapNextValue v) (Just a)
         -- install value updater on exit (if not already done)
         case mv of
            Just _  -> return () -- value updater already installed
            Nothing -> modifyTVar (snapContextUpdaters (snapContext v)) (updateSnapVar v:)

-- | SnapVar updater (on snapshot exit, for variable written during the snapshot)
updateSnapVar :: SnapVar a -> STM ()
updateSnapVar v = do
   -- read fresh value
   nv <- readTVar (snapNextValue v)
   writeTVar (snapNextValue v) Nothing
   case nv of
      Just val -> writeTVar (snapValue v) val
      Nothing  -> return () -- nothing to do, a writer has already done this before us

-- | Write a SnapVar
writeSnapVarIO :: MonadIO m => SnapVar a -> a -> m ()
writeSnapVarIO v a = atomically (writeSnapVar v a)

-- | Read a SnapVar
readSnapVar :: SnapVar a -> STM a
readSnapVar v = do
   state <- readTVar (snapContextState (snapContext v))
   case state of
      NoSnapshot -> readTVar (snapValue v)
      _          -> do
         -- in the SnapshotExit case we could update the value here if
         -- necessary, but an updater will eventually do it so we don't bother
         mv <- readTVar (snapNextValue v)
         case mv of
            Just a  -> return a
            Nothing -> readTVar (snapValue v)

-- | Read a SnapVar
readSnapVarIO :: MonadIO m => SnapVar a -> m a
readSnapVarIO v = atomically (readSnapVar v)

-- | Modify a SnapVar
modifySnapVar :: SnapVar a -> (a -> a) -> STM a
modifySnapVar v f = do
   old <- readSnapVar v
   writeSnapVar v (f old)
   return old

-- | Modify a SnapVar
modifySnapVarIO :: MonadIO m => SnapVar a -> (a -> a) -> m a
modifySnapVarIO v f = atomically (modifySnapVar v f)

-- | Read the snapshot value of the variable.
--
-- Must be used in the context of a `withSnapshot`
readSnapshotIO :: MonadIO m => SnapVar a -> m a
readSnapshotIO v = readTVarIO (snapValue v)

-- | Read the snapshot value of the variable.
--
-- Must be used in the context of a `withSnapshot`
readSnapshot :: SnapVar a -> STM a
readSnapshot v = readTVar (snapValue v)

-- | Use a snapshot
withSnapshot :: MonadIO m => SnapContext -> m r -> m r
withSnapshot ctx action = do
   -- enable snapshot
   old <- swapTVarIO (snapContextState ctx) Snapshot
   case old of
      NoSnapshot -> return ()
      _          -> error "withSnapshot: invalid snapshot state"

   
   -- use the snapshot
   r <- action

   -- finalize snapshot 
   writeTVarIO (snapContextState ctx) SnapshotExit -- in SnapshotExit state, no new updaters can be added to the list
   updaters <- readTVarIO (snapContextUpdaters ctx)
   forM_ updaters atomically -- run updaters one by one instead of all at once to have small transactions

   writeTVarIO (snapContextState ctx) NoSnapshot

   return r
