module Haskus.Memory.WriterM
  ( WriterM (..)
  , ReallocFun (..)
  , getUserState
  , writeM
  )
where

import GHC.Exts

import Haskus.Memory.Writer
import Haskus.Binary.Word

-- | A Writer monad
--
-- The Addr# is kept unboxed. When there isn't enough space left to writer, the
-- callback is called to allocate more.
newtype WriterM s b a
  = WriterM (WMState b s -> State# s -> (# State# s, WMState b s, a #))

type WMState b s
  = (# Addr#         -- current addres
    , U#             -- space left
    , ReallocFun b s -- realloc function
    , b #)           -- some extra user state

stateSpaceLeft :: WMState b s -> U#
stateSpaceLeft (# _, sz, _, _ #) = sz

stateRealloc :: WMState b s -> U# -> State# s -> (# State# s, WMState b s #)
stateRealloc c@(# _, _, ReallocFun f, _ #) sz = f sz c

stateUser :: WMState b s -> b
stateUser (# _, _, _, b #) = b

-- | Realloc function. It get passed the required space and the current state.
--
-- It must update allocated enough space and return the updated state (or maybe
-- throw an exception...).
--
-- This function can be used to keep a heap-object alive (e.g.  MutableByteArray
-- or ForeignPtr), just by capturing it.
newtype ReallocFun b s = ReallocFun (U# -> WMState b s -> State# s -> (# State# s, WMState b s #))

instance Functor (WriterM s b) where
  fmap f (WriterM m) = WriterM \c0 s0 ->
    let !(# s1, c1, a #) = m c0 s0
    in (# s1, c1, f a #)

instance Applicative (WriterM s b) where
  pure a = WriterM \c s -> (# s, c, a #)
  WriterM mf <*> WriterM mx = WriterM \c0 s0 ->
    let !(# s1, c1, f #) = mf c0 s0
        !(# s2, c2, x #) = mx c1 s1
    in (# s2, c2, f x #)

instance Monad (WriterM s b) where
  WriterM ma >>= f = WriterM \c0 s0 ->
    let !(# s1, c1, a #) = ma c0 s0
        (WriterM mb)     = f a
        !(# s2, c2, b #) = mb c1 s1
    in (# s2, c2, b #)


getUserState :: WriterM s b b
getUserState = WriterM \c s -> (# s, c, stateUser c #)

writeM :: SizedWriter s -> WriterM s b ()
writeM (SizedWriter sz w) = WriterM \c0 s0 ->
  if isTrue# (stateSpaceLeft c0 `geWord#` sz)
    then
      let !(# addr, space, realloc, b #) = c0
          !(# s1, addr' #) = runWriter# w addr s0
          !c1 = (# addr', space `minusWord#` sz, realloc, b #)
      in (# s1, c1, () #)
    else
      let !(# s1, c1 #) = stateRealloc c0 sz s0
          -- we assume enough space has been allocated, or an exception has been
          -- thrown.
          !(# addr, space, realloc, b #) = c1
          !(# s2, addr' #) = runWriter# w addr s1
          !c2 = (# addr', space `minusWord#` sz, realloc, b #)
      in (# s2, c2, () #)
