{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ImplicitParams #-}

-- | Continuation based control-flow
module Haskus.Utils.Variant.Cont
   ( fret
   , fretN
   , freturn
   , freturnN
   , frec
   -- * Control-flow
   , fIf
   , Then (..)
   , Else (..)
   )
where

import Haskus.Utils.Tuple
import Haskus.Utils.Types
import Haskus.Utils.ContFlow

-- this define has to be defined in each module using ContFlow for now
#define fdo ContFlow $ \__cs -> let ?__cs = __cs in do

-- | Call the type-indexed continuation from the tuple passed as first parameter
fret :: forall x r t n xs.
   ( ExtractTuple n t
   , xs ~ ContTupleToList t r
   , CheckMember x xs
   , n ~ IndexOf x xs
   , KnownNat n
   , CheckNub xs
   , Index n t ~ (x -> r)
   ) => Tuple t -> (x -> r)
{-# INLINABLE fret #-}
fret = tupleN @n

-- | Implicitly call the type-indexed continuation in the context
freturn :: forall x r t n xs.
   ( ExtractTuple n t
   , xs ~ ContTupleToList t r
   , CheckMember x xs
   , n ~ IndexOf x xs
   , KnownNat n
   , CheckNub xs
   , ?__cs :: Tuple t
   , Index n t ~ (x -> r)
   ) => x -> r
{-# INLINABLE freturn #-}
freturn = fret ?__cs

-- | Call the indexed continuation from the tuple passed as first parameter
fretN :: forall n x r t xs.
   ( ExtractTuple n t
   , xs ~ ContTupleToList t r
   , x ~ Index n xs
   , KnownNat n
   , Index n t ~ (x -> r)
   ) => Tuple t -> (x -> r)
{-# INLINABLE fretN #-}
fretN = tupleN @n


-- | Implicitly call the type-indexed continuation in the context
freturnN :: forall n x r t xs.
   ( ExtractTuple n t
   , xs ~ ContTupleToList t r
   , x ~ Index n xs
   , KnownNat n
   , ?__cs :: Tuple t
   , Index n t ~ (x -> r)
   ) => x -> r
{-# INLINABLE freturnN #-}
freturnN = fretN @n ?__cs


-- | Recursive call
frec :: forall r xs.
   ( ?__cs :: ContListToTuple xs r
   ) => ContFlow xs r -> r
frec f = f >::> ?__cs


----------------------------------------
-- Control-flow

data Then = Then
data Else = Else

fIf :: Bool -> ContFlow '[Then,Else] r
{-# INLINABLE fIf #-}
fIf b = fdo
   case b of
      True  -> freturn Then
      False -> freturn Else
