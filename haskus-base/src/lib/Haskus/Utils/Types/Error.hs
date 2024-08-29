{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}

-- | Type-level Error
module Haskus.Utils.Types.Error
   ( TypeError
   , ErrorMessage (..)
   , Assert
   )
where

import GHC.TypeLits

-- | Like: If cond t (TypeError msg)
--
-- The difference is that the TypeError doesn't appear in the RHS of the type
-- which leads to better error messages (see GHC #14771).
--
-- For instance:
--    type family F n where
--       F n = If (n <=? 8) Int8 (TypeError (Text "ERROR"))
--
--    type family G n where
--       G n = Assert (n <=? 8) Int8 (Text "ERROR")
--
--    If GHC cannot solve `F n ~ Word`, it shows: ERROR
--    If GHC cannot solve `G n ~ Word`, it shows:
--       can't match `Assert...` with `Word`
--
type family Assert (prop :: Bool) (val :: k) (msg :: ErrorMessage) :: k where
   Assert 'True  val msg = val
   Assert 'False val msg = TypeError msg

