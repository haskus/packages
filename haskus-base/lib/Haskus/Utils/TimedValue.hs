-- | Mutable value with associated last write time
module Haskus.Utils.TimedValue
   ( TimedValue (..)
   )
where

-- | Value with Eq/Ord instances uniquely based on time field indicating the
-- time the value was last written.
--
-- This can be used with IOVar/IOTree which use Eq instances to detect value
-- changes. It can be useful for values that we don't want to structurally
-- compare (because it is too costly or because we can't)
--
-- `t` should be SystemTime (fast to query monotonic clock)
data TimedValue t a = TimedValue t a

instance Eq t => Eq (TimedValue t a) where
   TimedValue t1 _ == TimedValue t2 _ = t1 == t2

instance Ord t => Ord (TimedValue t a) where
   compare (TimedValue t1 _) (TimedValue t2 _) = compare t1 t2
