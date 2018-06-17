{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Variant based control-flow
module Haskus.Utils.Variant.Flow
   ( Flow
   , IOV
   -- * Flow utils
   , flowRes
   , flowSingle
   , flowSetN
   , flowSet
   , flowLift
   , flowToCont
   , flowTraverse
   , flowFor
   , flowTraverseFilter
   , flowForFilter
   , Liftable
   , Popable
   , MaybePopable
   -- * Functor, applicative equivalents
   , (<$<)
   , (<*<)
   , (<|<)
   -- * Named operators
   , flowMap
   , flowBind
   , flowBind'
   , flowMatch
   , flowMatchFail
   -- * Operation on first element
   , (.~.>)
   , (>.~.>)
   , (.~+>)
   , (>.~+>)
   , (.~^^>)
   , (>.~^^>)
   , (.~^>)
   , (>.~^>)
   , (.~$>)
   , (>.~$>)
   , (.~|>)
   , (>.~|>)
   , (.~=>)
   , (>.~=>)
   , (.~!>)
   , (>.~!>)
   , (.~!!>)
   , (>.~!!>)
   -- ** Pure
   , (.-.>)
   , (>.-.>)
   , (<.-.)
   , (<.-.<)
   -- ** Const
   , (.~~.>)
   , (>.~~.>)
   , (.~~+>)
   , (>.~~+>)
   , (.~~^^>)
   , (>.~~^^>)
   , (.~~^>)
   , (>.~~^>)
   , (.~~$>)
   , (>.~~$>)
   , (.~~|>)
   , (>.~~|>)
   , (.~~=>)
   , (>.~~=>)
   , (.~~!>)
   , (>.~~!>)
   -- * Operation on tail
   , (..~.>)
   , (>..~.>)
   , (..-.>)
   , (>..-.>)
   , (..-..>)
   , (>..-..>)
   , (..~..>)
   , (>..~..>)
   , (..~^^>)
   , (>..~^^>)
   , (..~^>)
   , (>..~^>)
   , (..~=>)
   , (>..~=>)
   , (..~!>)
   , (>..~!>)
   , (..~!!>)
   , (>..~!!>)
   -- * Operation on caught element in tail
   , (..%~^>)
   , (>..%~^>)
   , (..%~^^>)
   , (>..%~^^>)
   , (..%~$>)
   , (>..%~$>)
   , (..%~!!>)
   , (>..%~!!>)
   , (..%~!>)
   , (>..%~!>)
   , (..?~^>)
   , (>..?~^>)
   , (..?~^^>)
   , (>..?~^^>)
   , (..?~$>)
   , (>..?~$>)
   , (..?~!!>)
   , (>..?~!!>)
   , (..?~!>)
   , (>..?~!>)
   -- * Operation on caught element
   , (%~.>)
   , (>%~.>)
   , (%~+>)
   , (>%~+>)
   , (%~^^>)
   , (>%~^^>)
   , (%~^>)
   , (>%~^>)
   , (%~$>)
   , (>%~$>)
   , (%~|>)
   , (>%~|>)
   , (%~=>)
   , (>%~=>)
   , (%~!>)
   , (>%~!>)
   , (%~!!>)
   , (>%~!!>)
   , (?~.>)
   , (>?~.>)
   , (?~+>)
   , (>?~+>)
   , (?~^^>)
   , (>?~^^>)
   , (?~^>)
   , (>?~^>)
   , (?~$>)
   , (>?~$>)
   , (?~|>)
   , (>?~|>)
   , (?~=>)
   , (>?~=>)
   , (?~!>)
   , (>?~!>)
   , (?~!!>)
   , (>?~!!>)
   -- * Operation on every element
   , (-||)
   , (-||>)
   , (>-||>)
   , (~||)
   , (~||>)
   , (>~||>)
   , LiftCont (..)
   , ExtractRHS
   , ReplaceRHS
   , LiftContTuple
   , ContVariant (..)
   -- * Helpers
   , makeFlowOp
   , makeFlowOpM
   , selectTail
   , selectFirst
   , selectType
   , applyConst
   , applyPure
   , applyM
   , applyF
   , combineFirst
   , combineSameTail
   , combineEither
   , combineConcat
   , combineUnion
   , combineLiftUnselected
   , combineLiftBoth
   , combineSingle
   , liftV
   , liftF
   )
where

import Haskus.Utils.Variant
import Haskus.Utils.Types
import Haskus.Utils.Types.List
import Haskus.Utils.ContFlow
import Haskus.Utils.Tuple

-- | Control-flow
type Flow m (l :: [*]) = m (Variant l)

type IOV l = Flow IO l

----------------------------------------------------------
-- Flow utils
----------------------------------------------------------

-- | Return in the first element
flowSetN :: forall (n :: Nat) xs m.
   ( Monad m
   , KnownNat n
   ) => Index n xs -> Flow m xs
{-# INLINE flowSetN #-}
flowSetN = return . toVariantAt @n

-- | Return in the first well-typed element
flowSet :: (Member x xs, Monad m) => x -> Flow m xs
{-# INLINE flowSet #-}
flowSet = return . toVariant

-- | Return a single element
flowSingle :: Monad m => x -> Flow m '[x]
{-# INLINE flowSingle #-}
flowSingle = flowSetN @0

-- | Lift a flow into another
flowLift :: (Liftable xs ys , Monad m) => Flow m xs -> Flow m ys
{-# INLINE flowLift #-}
flowLift = fmap liftVariant

-- | Lift a flow into a ContFlow
flowToCont :: (ContVariant xs, Monad m) => Flow m xs -> ContFlow xs (m r)
flowToCont = variantToContM

-- | Traverse a list and stop on first error
flowTraverse :: forall m a b xs.
   ( Monad m
   ) => (a -> Flow m (b ': xs)) -> [a] -> Flow m ([b] ': xs)
flowTraverse f = go (flowSetN @0 [])
   where
      go :: Flow m ([b] ': xs) -> [a] -> Flow m ([b] ': xs)
      go rs []     = rs >.-.> reverse
      go rs (a:as) = go rs' as
         where
            -- execute (f a) if previous execution succedded.
            -- prepend the result to the list
            rs' = rs >.~$> \bs -> (f a >.-.> (:bs))

-- | Traverse a list and stop on first error
flowFor :: forall m a b xs.
   ( Monad m
   ) => [a] -> (a -> Flow m (b ': xs)) -> Flow m ([b] ': xs)
flowFor = flip flowTraverse

-- | Traverse a list and return only valid values
flowTraverseFilter :: forall m a b xs.
   ( Monad m
   ) => (a -> Flow m (b ': xs)) -> [a] -> m [b]
flowTraverseFilter f = go
   where
      go :: [a] -> m [b]
      go []     = return []
      go (a:as) = do
         f a >.~.> (\b -> (b:) <$> go as)
             >..~.> const (go as)

-- | Traverse a list and return only valid values
flowForFilter :: forall m a b xs.
   ( Monad m
   ) => [a] -> (a -> Flow m (b ': xs)) -> m [b]
flowForFilter = flip flowTraverseFilter


-- | Extract single flow result
flowRes :: Functor m => Flow m '[x] -> m x
{-# INLINE flowRes #-}
flowRes = fmap variantToValue


-- | Lift an operation on a Variant into an operation on a flow
liftm :: Monad m => (Variant x -> a -> m b) -> Flow m x -> a -> m b
{-# INLINE liftm #-}
liftm op x a = do
   x' <- x
   op x' a

----------------------------------------------------------
-- Named operators
----------------------------------------------------------

-- | Map a pure function onto the correct value in the flow
flowMap :: Monad m => Flow m (x ': xs) -> (x -> y) -> Flow m (y ': xs)
{-# INLINE flowMap #-}
flowMap = (>.-.>)

-- | Bind two flows in a monadish way (error types union)
flowBind :: forall xs ys zs m x.
   ( Liftable xs zs
   , Liftable ys zs
   , zs ~ Union xs ys
   , Monad m
   ) => Flow m (x ': ys) -> (x -> Flow m xs) -> Flow m zs
{-# INLINE flowBind #-}
flowBind = (>.~|>)

-- | Bind two flows in a monadic way (constant error types)
flowBind' :: Monad m => Flow m (x ': xs) -> (x -> Flow m (y ': xs)) -> Flow m (y ': xs)
{-# INLINE flowBind' #-}
flowBind' = (>.~$>)

-- | Match a value in a flow
flowMatch :: forall x xs zs m.
   ( Monad m
   , Popable x xs
   , Liftable (Filter x xs) zs
   ) => Flow m xs -> (x -> Flow m zs) -> Flow m zs
{-# INLINE flowMatch #-}
flowMatch = (>%~^>)

-- | Match a value in a flow and use a non-returning failure in this case
flowMatchFail :: forall x xs m.
   ( Monad m
   , Popable x xs
   ) => Flow m xs -> (x -> m ()) -> Flow m (Filter x xs)
{-# INLINE flowMatchFail #-}
flowMatchFail = (>%~!!>)

----------------------------------------------------------
-- First element operations
----------------------------------------------------------

-- | Extract the first value, set the first value
(.~.>) :: forall m l x a.
   ( Monad m )
   => Variant (a ': l) -> (a -> m x) -> Flow m (x ': l)
{-# INLINE (.~.>) #-}
(.~.>) v f = makeFlowOp selectFirst (applyM f) combineFirst v

infixl 0 .~.>

-- | Extract the first value, set the first value
(>.~.>) :: forall m l x a.
   ( Monad m )
   => Flow m (a ': l) -> (a -> m x) -> Flow m (x ': l)
{-# INLINE (>.~.>) #-}
(>.~.>) = liftm (.~.>)

infixl 0 >.~.>

-- | Extract the first value, concat the result
(.~+>) :: forall (k :: Nat) m l l2 a.
   ( KnownNat k
   , k ~ Length l2
   , Monad m )
   => Variant (a ': l) -> (a -> Flow m l2) -> Flow m (Concat l2 l)
{-# INLINE (.~+>) #-}
(.~+>) v f = makeFlowOp selectFirst (applyF f) combineConcat v

infixl 0 .~+>

-- | Extract the first value, concat the results
(>.~+>) :: forall (k :: Nat) m l l2 a.
   ( KnownNat k
   , k ~ Length l2
   , Monad m )
   => Flow m (a ': l) -> (a -> Flow m l2) -> Flow m (Concat l2 l)
{-# INLINE (>.~+>) #-}
(>.~+>) = liftm (.~+>)

infixl 0 >.~+>

-- | Extract the first value, lift both
(.~^^>) :: forall m a xs ys zs.
   ( Monad m
   , Liftable xs zs
   , Liftable ys zs
   ) => Variant (a ': ys) -> (a -> Flow m xs) -> Flow m zs
{-# INLINE (.~^^>) #-}
(.~^^>) v f = makeFlowOp selectFirst (applyF f) combineLiftBoth v

infixl 0 .~^^>


-- | Extract the first value, lift both
(>.~^^>) :: forall m a xs ys zs.
   ( Monad m
   , Liftable xs zs
   , Liftable ys zs
   ) => Flow m (a ': ys) -> (a -> Flow m xs) -> Flow m zs
{-# INLINE (>.~^^>) #-}
(>.~^^>) = liftm (.~^^>)

infixl 0 >.~^^>

-- | Extract the first value, lift unselected
(.~^>) :: forall m a ys zs.
   ( Monad m
   , Liftable ys zs
   ) => Variant (a ': ys) -> (a -> Flow m zs) -> Flow m zs
{-# INLINE (.~^>) #-}
(.~^>) v f = makeFlowOp selectFirst (applyF f) combineLiftUnselected v

infixl 0 .~^>

-- | Extract the first value, lift unselected
(>.~^>) :: forall m a ys zs.
   ( Monad m
   , Liftable ys zs
   ) => Flow m (a ': ys) -> (a -> Flow m zs) -> Flow m zs
{-# INLINE (>.~^>) #-}
(>.~^>) = liftm (.~^>)

infixl 0 >.~^>

-- | Extract the first value, use the same tail
(.~$>) :: forall m x xs a.
   ( Monad m
   ) => Variant (a ': xs) -> (a -> Flow m (x ': xs)) -> Flow m (x ': xs)
{-# INLINE (.~$>) #-}
(.~$>) v f = makeFlowOp selectFirst (applyF f) combineSameTail v

infixl 0 .~$>

-- | Extract the first value, use the same tail
(>.~$>) :: forall m x xs a.
   ( Monad m
   ) => Flow m (a ': xs) -> (a -> Flow m (x ': xs)) -> Flow m (x ': xs)
{-# INLINE (>.~$>) #-}
(>.~$>) = liftm (.~$>)

infixl 0 >.~$>

-- | Take the first output, union the result
(.~|>) ::
   ( Liftable xs zs
   , Liftable ys zs
   , zs ~ Union xs ys
   , Monad m
   ) => Variant (a ': ys) -> (a -> Flow m xs) -> Flow m zs
{-# INLINE (.~|>) #-}
(.~|>) v f = makeFlowOp selectFirst (applyF f) combineUnion v

infixl 0 .~|>

-- | Take the first output, fusion the result
(>.~|>) ::
   ( Liftable xs zs
   , Liftable ys zs
   , zs ~ Union xs ys
   , Monad m
   ) => Flow m (a ': ys) -> (a -> Flow m xs) -> Flow m zs
{-# INLINE (>.~|>) #-}
(>.~|>) = liftm (.~|>)

infixl 0 >.~|>

-- | Extract the first value and perform effect. Passthrough the input value
(.~=>) ::
   ( Monad m
   ) => Variant (a ': l) -> (a -> m ()) -> Flow m (a ': l)
{-# INLINE (.~=>) #-}
(.~=>) v f = case popVariantHead v of
   Right u -> f u >> return v
   Left  _ -> return v

infixl 0 .~=>

-- | Extract the first value and perform effect. Passthrough the input value
(>.~=>) ::
   ( Monad m
   ) => Flow m (a ': l) -> (a -> m ()) -> Flow m (a ': l)
{-# INLINE (>.~=>) #-}
(>.~=>) = liftm (.~=>)

infixl 0 >.~=>

-- | Extract the first value and perform effect.
(.~!>) ::
   ( Monad m
   ) => Variant (a ': l) -> (a -> m ()) -> m ()
{-# INLINE (.~!>) #-}
(.~!>) v f = case popVariantHead v of
   Right u -> f u
   Left  _ -> return ()

infixl 0 .~!>

-- | Extract the first value and perform effect.
(>.~!>) ::
   ( Monad m
   ) => Flow m (a ': l) -> (a -> m ()) -> m ()
{-# INLINE (>.~!>) #-}
(>.~!>) = liftm (.~!>)

infixl 0 >.~!>

-- | Extract the first value and perform effect.
(.~!!>) ::
   ( Monad m
   ) => Variant (a ': l) -> (a -> m ()) -> m (Variant l)
{-# INLINE (.~!!>) #-}
(.~!!>) v f = case popVariantHead v of
   Right u -> f u >> error ".~!!> error"
   Left  l -> return l

infixl 0 .~!!>

-- | Extract the first value and perform effect.
(>.~!!>) ::
   ( Monad m
   ) => Flow m (a ': l) -> (a -> m ()) -> m (Variant l)
{-# INLINE (>.~!!>) #-}
(>.~!!>) = liftm (.~!!>)

infixl 0 >.~!!>

----------------------------------------------------------
-- First element, pure variant
----------------------------------------------------------

-- | Extract the first value, set the first value
(.-.>) :: forall m l x a.
   ( Monad m )
   => Variant (a ': l) -> (a -> x) -> Flow m (x ': l)
{-# INLINE (.-.>) #-}
(.-.>) v f = makeFlowOp selectFirst (applyPure (liftV f)) combineFirst v

infixl 0 .-.>

-- | Extract the first value, set the first value
(>.-.>) :: forall m l x a.
   ( Monad m )
   => Flow m (a ': l) -> (a -> x) -> Flow m (x ': l)
{-# INLINE (>.-.>) #-}
(>.-.>) = liftm (.-.>)

infixl 0 >.-.>

-- | Extract the first value, set the first value
(<.-.) :: forall m l x a.
   ( Monad m )
   => (a -> x) -> Variant (a ': l) -> Flow m (x ': l)
{-# INLINE (<.-.) #-}
(<.-.) = flip (.-.>)

infixr 0 <.-.

-- | Extract the first value, set the first value
(<.-.<) :: forall m l x a.
   ( Monad m )
   => (a -> x) -> Flow m (a ': l) -> Flow m (x ': l)
{-# INLINE (<.-.<) #-}
(<.-.<) = flip (>.-.>)

infixr 0 <.-.<

----------------------------------------------------------
-- Functor, applicative
----------------------------------------------------------

-- | Functor <$> equivalent
(<$<) :: forall m l a b.
   ( Monad m )
   => (a -> b) -> Flow m (a ': l) -> Flow m (b ': l)
{-# INLINE (<$<) #-}
(<$<) = (<.-.<)

infixl 4 <$<

-- | Applicative <*> equivalent
(<*<) :: forall m l a b.
   ( Monad m )
   => Flow m ((a -> b) ': l) -> Flow m (a ': l) -> Flow m (b ': l)
{-# INLINE (<*<) #-}
(<*<) mf mg = mf >.~$> (mg >.-.>)

infixl 4 <*<

-- | Applicative <*> equivalent, with error union
(<|<) :: forall m xs ys zs y z.
   ( Monad m
   , Liftable xs zs
   , Liftable ys zs
   , zs ~ Union xs ys
   ) => Flow m ((y -> z) ': xs) -> Flow m (y ': ys) -> Flow m (z ': zs)
{-# INLINE (<|<) #-}
(<|<) mf mg = 
   mf >..-..> liftVariant
      >.~$> (\f -> mg >..-..> liftVariant
                      >.-.> f
            )

infixl 4 <|<

----------------------------------------------------------
-- First element, const variant
----------------------------------------------------------

-- | Extract the first value, set the first value
(.~~.>) :: forall m l x a.
   ( Monad m )
   => Variant (a ': l) -> m x -> Flow m (x ': l)
{-# INLINE (.~~.>) #-}
(.~~.>) v f = v .~.> const f

infixl 0 .~~.>

-- | Extract the first value, set the first value
(>.~~.>) :: forall m l x a.
   ( Monad m )
   => Flow m (a ': l) -> m x -> Flow m (x ': l)
{-# INLINE (>.~~.>) #-}
(>.~~.>) = liftm (.~~.>)

infixl 0 >.~~.>

-- | Extract the first value, concat the result
(.~~+>) :: forall (k :: Nat) m l l2 a.
   ( KnownNat k
   , k ~ Length l2
   , Monad m )
   => Variant (a ': l) -> Flow m l2 -> Flow m (Concat l2 l)
{-# INLINE (.~~+>) #-}
(.~~+>) v f = v .~+> const f

infixl 0 .~~+>

-- | Extract the first value, concat the results
(>.~~+>) :: forall (k :: Nat) m l l2 a.
   ( KnownNat k
   , k ~ Length l2
   , Monad m )
   => Flow m (a ': l) -> Flow m l2 -> Flow m (Concat l2 l)
{-# INLINE (>.~~+>) #-}
(>.~~+>) = liftm (.~~+>)

infixl 0 >.~~+>

-- | Extract the first value, lift the result
(.~~^^>) :: forall m a xs ys zs.
   ( Monad m
   , Liftable xs zs
   , Liftable ys zs
   ) => Variant (a ': ys) -> Flow m xs -> Flow m zs
{-# INLINE (.~~^^>) #-}
(.~~^^>) v f = v .~^^> const f

infixl 0 .~~^^>


-- | Extract the first value, lift the result
(>.~~^^>) :: forall m a xs ys zs.
   ( Monad m
   , Liftable xs zs
   , Liftable ys zs
   ) => Flow m (a ': ys) -> Flow m xs -> Flow m zs
{-# INLINE (>.~~^^>) #-}
(>.~~^^>) = liftm (.~~^^>)

infixl 0 >.~~^^>

-- | Extract the first value, connect to the expected output
(.~~^>) :: forall m a ys zs.
   ( Monad m
   , Liftable ys zs
   ) => Variant (a ': ys) -> Flow m zs -> Flow m zs
{-# INLINE (.~~^>) #-}
(.~~^>) v f = v .~^> const f

infixl 0 .~~^>

-- | Extract the first value, connect to the expected output
(>.~~^>) :: forall m a ys zs.
   ( Monad m
   , Liftable ys zs
   ) => Flow m (a ': ys) -> Flow m zs -> Flow m zs
{-# INLINE (>.~~^>) #-}
(>.~~^>) = liftm (.~~^>)

infixl 0 >.~~^>

-- | Extract the first value, use the same output type
(.~~$>) :: forall m x xs a.
   ( Monad m
   ) => Variant (a ': xs) -> Flow m (x ': xs) -> Flow m (x ': xs)
{-# INLINE (.~~$>) #-}
(.~~$>) v f = v .~$> const f

infixl 0 .~~$>

-- | Extract the first value, use the same output type
(>.~~$>) :: forall m x xs a.
   ( Monad m
   ) => Flow m (a ': xs) -> Flow m (x ': xs) -> Flow m (x ': xs)
{-# INLINE (>.~~$>) #-}
(>.~~$>) = liftm (.~~$>)

infixl 0 >.~~$>

-- | Take the first output, fusion the result
(.~~|>) ::
   ( Liftable xs zs
   , Liftable ys zs
   , zs ~ Union xs ys
   , Monad m
   ) => Variant (a ': ys) -> Flow m xs -> Flow m zs
{-# INLINE (.~~|>) #-}
(.~~|>) v f = v .~|> const f

infixl 0 .~~|>

-- | Take the first output, fusion the result
(>.~~|>) ::
   ( Liftable xs zs
   , Liftable ys zs
   , zs ~ Union xs ys
   , Monad m
   ) => Flow m (a ': ys) -> Flow m xs -> Flow m zs
{-# INLINE (>.~~|>) #-}
(>.~~|>) = liftm (.~~|>)

infixl 0 >.~~|>

-- | Extract the first value and perform effect. Passthrough the input value
(.~~=>) ::
   ( Monad m
   ) => Variant (a ': l) -> m () -> Flow m (a ': l)
{-# INLINE (.~~=>) #-}
(.~~=>) v f = v .~=> const f

infixl 0 .~~=>

-- | Extract the first value and perform effect. Passthrough the input value
(>.~~=>) ::
   ( Monad m
   ) => Flow m (a ': l) -> m () -> Flow m (a ': l)
{-# INLINE (>.~~=>) #-}
(>.~~=>) = liftm (.~~=>)

infixl 0 >.~~=>

-- | Extract the first value and perform effect.
(.~~!>) ::
   ( Monad m
   ) => Variant (a ': l) -> m () -> m ()
{-# INLINE (.~~!>) #-}
(.~~!>) v f = v .~!> const f

infixl 0 .~~!>

-- | Extract the first value and perform effect.
(>.~~!>) ::
   ( Monad m
   ) => Flow m (a ': l) -> m () -> m ()
{-# INLINE (>.~~!>) #-}
(>.~~!>) = liftm (.~~!>)

infixl 0 >.~~!>


----------------------------------------------------------
-- Tail operations
----------------------------------------------------------

-- | Extract the tail, set the first value
(..~.>) ::
   ( Monad m
   ) => Variant (a ': l) -> (Variant l -> m a) -> m a
{-# INLINE (..~.>) #-}
(..~.>) v f = makeFlowOp selectTail (applyVM f) combineSingle v

infixl 0 ..~.>

-- | Extract the tail, set the first value
(>..~.>) ::
   ( Monad m
   ) => Flow m (a ': l) -> (Variant l -> m a) -> m a
{-# INLINE (>..~.>) #-}
(>..~.>) = liftm (..~.>)

infixl 0 >..~.>

-- | Extract the tail, set the first value (pure function)
(..-.>) ::
   ( Monad m
   ) => Variant (a ': l) -> (Variant l -> a) -> m a
{-# INLINE (..-.>) #-}
(..-.>) v f = case popVariantHead v of
   Right u -> return u
   Left  l -> return (f l)

infixl 0 ..-.>

-- | Extract the tail, set the first value (pure function)
(>..-.>) ::
   ( Monad m
   ) => Flow m (a ': l) -> (Variant l -> a) -> m a
{-# INLINE (>..-.>) #-}
(>..-.>) = liftm (..-.>)

infixl 0 >..-.>

-- | Extract the tail, set the tail
(..-..>) :: forall a l xs m.
   ( Monad m
   ) => Variant (a ': l) -> (Variant l -> Variant xs) -> Flow m (a ': xs)
{-# INLINE (..-..>) #-}
(..-..>) v f = case popVariantHead v of
   Right u -> flowSetN @0 u
   Left  l -> return (prependVariant @'[a] (f l))

infixl 0 ..-..>

-- | Extract the tail, set the tail
(>..-..>) ::
   ( Monad m
   ) => Flow m (a ': l) -> (Variant l -> Variant xs) -> Flow m (a ': xs)
{-# INLINE (>..-..>) #-}
(>..-..>) = liftm (..-..>)

infixl 0 >..-..>

-- | Extract the tail, set the tail
(..~..>) :: forall a l xs m.
   ( Monad m
   ) => Variant (a ': l) -> (Variant l -> Flow m xs) -> Flow m (a ': xs)
{-# INLINE (..~..>) #-}
(..~..>) v f = case popVariantHead v of
   Right u -> flowSetN @0 u
   Left  l -> prependVariant @'[a] <$> f l

infixl 0 ..~..>

-- | Extract the tail, set the tail
(>..~..>) ::
   ( Monad m
   ) => Flow m (a ': l) -> (Variant l -> Flow m xs) -> Flow m (a ': xs)
{-# INLINE (>..~..>) #-}
(>..~..>) = liftm (..~..>)

infixl 0 >..~..>

-- | Extract the tail, lift the result
(..~^^>) ::
   ( Monad m
   , Liftable xs (a ': zs)
   ) => Variant (a ': l) -> (Variant l -> Flow m xs) -> Flow m (a ': zs)
{-# INLINE (..~^^>) #-}
(..~^^>) v f = case popVariantHead v of
   Right u -> flowSetN @0 u
   Left  l -> liftVariant <$> f l

infixl 0 ..~^^>

-- | Extract the tail, lift the result
(>..~^^>) ::
   ( Monad m
   , Liftable xs (a ': zs)
   ) => Flow m  (a ': l) -> (Variant l -> Flow m xs) -> Flow m (a ': zs)
{-# INLINE (>..~^^>) #-}
(>..~^^>) = liftm (..~^^>)

infixl 0 >..~^^>

-- | Extract the tail, connect the result
(..~^>) ::
   ( Monad m
   , Member a zs
   ) => Variant (a ': l) -> (Variant l -> Flow m zs) -> Flow m zs
{-# INLINE (..~^>) #-}
(..~^>) v f = case popVariantHead v of
   Right u -> flowSet u
   Left  l -> f l

infixl 0 ..~^>

-- | Extract the tail, connect the result
(>..~^>) ::
   ( Monad m
   , Member a zs
   ) => Flow m (a ': l) -> (Variant l -> Flow m zs) -> Flow m zs
{-# INLINE (>..~^>) #-}
(>..~^>) = liftm (..~^>)

infixl 0 >..~^>

-- | Match in the tail, connect to the expected result
(..?~^>) ::
   ( Monad m
   , MaybePopable a xs
   , Liftable (Filter a xs) ys
   ) => Variant (x ': xs) -> (a -> Flow m ys) -> Flow m (x ': ys)
{-# INLINE (..?~^>) #-}
(..?~^>) v f = v ..~..> (\v' -> v' ?~^> f)

infixl 0 ..?~^>

-- | Match in the tail, connect to the expected result
(>..?~^>) ::
   ( Monad m
   , MaybePopable a xs
   , Liftable (Filter a xs) ys
   ) => Flow m (x ': xs) -> (a -> Flow m ys) -> Flow m (x ': ys)
{-# INLINE (>..?~^>) #-}
(>..?~^>) = liftm (..?~^>)

infixl 0 >..?~^>

-- | Match in the tail, connect to the expected result
(..%~^>) ::
   ( Monad m
   , Popable a xs
   , Liftable (Filter a xs) ys
   ) => Variant (x ': xs) -> (a -> Flow m ys) -> Flow m (x ': ys)
{-# INLINE (..%~^>) #-}
(..%~^>) v f = v ..~..> (\v' -> v' %~^> f)

infixl 0 ..%~^>

-- | Match in the tail, connect to the expected result
(>..%~^>) ::
   ( Monad m
   , Popable a xs
   , Liftable (Filter a xs) ys
   ) => Flow m (x ': xs) -> (a -> Flow m ys) -> Flow m (x ': ys)
{-# INLINE (>..%~^>) #-}
(>..%~^>) = liftm (..%~^>)

infixl 0 >..%~^>

-- | Match in the tail, lift to the expected result
(..?~^^>) ::
   ( Monad m
   , MaybePopable a xs
   , Liftable (Filter a xs) zs
   , Liftable ys zs
   ) => Variant (x ': xs) -> (a -> Flow m ys) -> Flow m (x ': zs)
{-# INLINE (..?~^^>) #-}
(..?~^^>) v f = v ..~..> (\v' -> v' ?~^^> f)

infixl 0 ..?~^^>

-- | Match in the tail, lift to the expected result
(>..?~^^>) ::
   ( Monad m
   , MaybePopable a xs
   , Liftable (Filter a xs) zs
   , Liftable ys zs
   ) => Flow m (x ': xs) -> (a -> Flow m ys) -> Flow m (x ': zs)
{-# INLINE (>..?~^^>) #-}
(>..?~^^>) = liftm (..?~^^>)

infixl 0 >..?~^^>

-- | Match in the tail, lift to the expected result
(..%~^^>) ::
   ( Monad m
   , Popable a xs
   , Liftable (Filter a xs) zs
   , Liftable ys zs
   ) => Variant (x ': xs) -> (a -> Flow m ys) -> Flow m (x ': zs)
{-# INLINE (..%~^^>) #-}
(..%~^^>) v f = v ..~..> (\v' -> v' %~^^> f)

infixl 0 ..%~^^>

-- | Match in the tail, lift to the expected result
(>..%~^^>) ::
   ( Monad m
   , Popable a xs
   , Liftable (Filter a xs) zs
   , Liftable ys zs
   ) => Flow m (x ': xs) -> (a -> Flow m ys) -> Flow m (x ': zs)
{-# INLINE (>..%~^^>) #-}
(>..%~^^>) = liftm (..%~^^>)

infixl 0 >..%~^^>

-- | Match in the tail, keep the same types
(..?~$>) ::
   ( Monad m
   , MaybePopable a xs
   , Liftable (Filter a xs) (x ': xs)
   ) => Variant (x ': xs) -> (a -> Flow m (x ': xs)) -> Flow m (x ': xs)
{-# INLINE (..?~$>) #-}
(..?~$>) v f = case popVariantHead v of
   Right _ -> return v
   Left xs -> xs ?~^> f

infixl 0 ..?~$>

-- | Match in the tail, keep the same types
(>..?~$>) ::
   ( Monad m
   , MaybePopable a xs
   , Liftable (Filter a xs) (x ': xs)
   ) => Flow m (x ': xs) -> (a -> Flow m (x ': xs)) -> Flow m (x ': xs)
{-# INLINE (>..?~$>) #-}
(>..?~$>) = liftm (..?~$>)

infixl 0 >..?~$>

-- | Match in the tail, keep the same types
(..%~$>) ::
   ( Monad m
   , Popable a xs
   , Liftable (Filter a xs) (x ': xs)
   ) => Variant (x ': xs) -> (a -> Flow m (x ': xs)) -> Flow m (x ': xs)
{-# INLINE (..%~$>) #-}
(..%~$>) v f = case popVariantHead v of
   Right _ -> return v
   Left xs -> xs %~^> f

infixl 0 ..%~$>

-- | Match in the tail, keep the same types
(>..%~$>) ::
   ( Monad m
   , Popable a xs
   , Liftable (Filter a xs) (x ': xs)
   ) => Flow m (x ': xs) -> (a -> Flow m (x ': xs)) -> Flow m (x ': xs)
{-# INLINE (>..%~$>) #-}
(>..%~$>) = liftm (..%~$>)

infixl 0 >..%~$>


-- | Extract the tail and perform an effect. Passthrough the input value
(..~=>) ::
   ( Monad m
   ) => Variant (x ': xs) -> (Variant xs -> m ()) -> Flow m (x ': xs)
{-# INLINE (..~=>) #-}
(..~=>) v f = case popVariantHead v of
   Right _ -> return v
   Left  l -> f l >> return v

infixl 0 ..~=>

-- | Extract the tail and perform an effect. Passthrough the input value
(>..~=>) ::
   ( Monad m
   ) => Flow m (x ': xs) -> (Variant xs -> m ()) -> Flow m (x ': xs)
{-# INLINE (>..~=>) #-}
(>..~=>) = liftm (..~=>)

infixl 0 >..~=>

-- | Extract the tail and perform an effect
(..~!>) ::
   ( Monad m
   ) => Variant (x ': xs) -> (Variant xs -> m ()) -> m ()
{-# INLINE (..~!>) #-}
(..~!>) v f = case popVariantHead v of
   Right _ -> return ()
   Left  l -> f l

infixl 0 ..~!>

-- | Extract the tail and perform an effect
(>..~!>) ::
   ( Monad m
   ) => Flow m (x ': xs) -> (Variant xs -> m ()) -> m ()
{-# INLINE (>..~!>) #-}
(>..~!>) = liftm (..~!>)

infixl 0 >..~!>

-- | Extract the tail and perform an effect
(..~!!>) ::
   ( Monad m
   ) => Variant (x ': xs) -> (Variant xs -> m ()) -> m x
{-# INLINE (..~!!>) #-}
(..~!!>) v f = case popVariantHead v of
   Right x -> return x
   Left xs -> f xs >> error "..~!!> error"

infixl 0 ..~!!>

-- | Extract the tail and perform an effect
(>..~!!>) ::
   ( Monad m
   ) => Flow m (x ': xs) -> (Variant xs -> m ()) -> m x
{-# INLINE (>..~!!>) #-}
(>..~!!>) = liftm (..~!!>)

infixl 0 >..~!!>

-- | Match in the tail and perform an effect
(..?~!!>) ::
   ( Monad m
   , MaybePopable y xs
   ) => Variant (x ': xs) -> (y -> m ()) -> Flow m (x ': Filter y xs)
{-# INLINE (..?~!!>) #-}
(..?~!!>) v f = v ..~..> (\xs -> xs ?~!!> f)

infixl 0 ..?~!!>

-- | Match in the tail and perform an effect
(>..?~!!>) ::
   ( Monad m
   , MaybePopable y xs
   ) => Flow m (x ': xs) -> (y -> m ()) -> Flow m (x ': Filter y xs)
{-# INLINE (>..?~!!>) #-}
(>..?~!!>) = liftm (..?~!!>)

infixl 0 >..?~!!>

-- | Match in the tail and perform an effect
(..%~!!>) ::
   ( Monad m
   , Popable y xs
   ) => Variant (x ': xs) -> (y -> m ()) -> Flow m (x ': Filter y xs)
{-# INLINE (..%~!!>) #-}
(..%~!!>) v f = v ..~..> (\xs -> xs %~!!> f)

infixl 0 ..%~!!>

-- | Match in the tail and perform an effect
(>..%~!!>) ::
   ( Monad m
   , Popable y xs
   ) => Flow m (x ': xs) -> (y -> m ()) -> Flow m (x ': Filter y xs)
{-# INLINE (>..%~!!>) #-}
(>..%~!!>) = liftm (..%~!!>)

infixl 0 >..%~!!>

-- | Match in the tail and perform an effect
(..?~!>) ::
   ( Monad m
   , MaybePopable y xs
   ) => Variant (x ': xs) -> (y -> m ()) -> m ()
{-# INLINE (..?~!>) #-}
(..?~!>) v f = case popVariantHead v of
   Right _ -> return ()
   Left xs -> xs ?~!> f

infixl 0 ..?~!>

-- | Match in the tail and perform an effect
(>..?~!>) ::
   ( Monad m
   , MaybePopable y xs
   ) => Flow m (x ': xs) -> (y -> m ()) -> m ()
{-# INLINE (>..?~!>) #-}
(>..?~!>) = liftm (..?~!>)

infixl 0 >..?~!>

-- | Match in the tail and perform an effect
(..%~!>) ::
   ( Monad m
   , Popable y xs
   ) => Variant (x ': xs) -> (y -> m ()) -> m ()
{-# INLINE (..%~!>) #-}
(..%~!>) v f = case popVariantHead v of
   Right _ -> return ()
   Left xs -> xs %~!> f

infixl 0 ..%~!>

-- | Match in the tail and perform an effect
(>..%~!>) ::
   ( Monad m
   , Popable y xs
   ) => Flow m (x ': xs) -> (y -> m ()) -> m ()
{-# INLINE (>..%~!>) #-}
(>..%~!>) = liftm (..%~!>)

infixl 0 >..%~!>

----------------------------------------------------------
-- Caught element operations
----------------------------------------------------------

-- | Pop element, set the first value
(?~.>) :: forall x xs y ys m.
   ( ys ~ Filter x xs
   , Monad m
   , MaybePopable x xs
   ) => Variant xs -> (x -> m y) -> Flow m (y ': ys)
{-# INLINE (?~.>) #-}
(?~.>) v f = case popVariantMaybe v of
   Right x -> flowSetN @0 =<< f x
   Left ys -> prependVariant @'[y] <$> return ys

infixl 0 ?~.>

-- | Pop element, set the first value
(>?~.>) ::
   ( ys ~ Filter x xs
   , Monad m
   , MaybePopable x xs
   ) => Flow m xs -> (x -> m y) -> Flow m (y ': ys)
{-# INLINE (>?~.>) #-}
(>?~.>) = liftm (?~.>)

infixl 0 >?~.>

-- | Pop element, set the first value
(%~.>) :: forall x xs y ys m.
   ( ys ~ Filter x xs
   , Monad m
   , Popable x xs
   ) => Variant xs -> (x -> m y) -> Flow m (y ': ys)
{-# INLINE (%~.>) #-}
(%~.>) = (?~.>)

infixl 0 %~.>

-- | Pop element, set the first value
(>%~.>) ::
   ( ys ~ Filter x xs
   , Monad m
   , Popable x xs
   ) => Flow m xs -> (x -> m y) -> Flow m (y ': ys)
{-# INLINE (>%~.>) #-}
(>%~.>) = liftm (%~.>)

infixl 0 >%~.>

-- | Pop element, concat the result
(?~+>) :: forall x xs ys m.
   ( Monad m
   , MaybePopable x xs
   , KnownNat (Length ys)
   ) => Variant xs -> (x -> Flow m ys) -> Flow m (Concat ys (Filter x xs))
{-# INLINE (?~+>) #-}
(?~+>) v f = case popVariantMaybe v of
   Right x -> appendVariant  @(Filter x xs) <$> f x
   Left ys -> prependVariant @ys            <$> return ys

infixl 0 ?~+>

-- | Pop element, concat the result
(>?~+>) :: forall x xs ys m.
   ( Monad m
   , MaybePopable x xs
   , KnownNat (Length ys)
   ) => Flow m xs -> (x -> Flow m ys) -> Flow m (Concat ys (Filter x xs))
{-# INLINE (>?~+>) #-}
(>?~+>) = liftm (?~+>)

infixl 0 >?~+>

-- | Pop element, concat the result
(%~+>) :: forall x xs ys m.
   ( Monad m
   , Popable x xs
   , KnownNat (Length ys)
   ) => Variant xs -> (x -> Flow m ys) -> Flow m (Concat ys (Filter x xs))
{-# INLINE (%~+>) #-}
(%~+>) = (?~+>)

infixl 0 %~+>

-- | Pop element, concat the result
(>%~+>) :: forall x xs ys m.
   ( Monad m
   , Popable x xs
   , KnownNat (Length ys)
   ) => Flow m xs -> (x -> Flow m ys) -> Flow m (Concat ys (Filter x xs))
{-# INLINE (>%~+>) #-}
(>%~+>) = liftm (%~+>)

infixl 0 >%~+>

-- | Pop element, lift the result
(?~^^>) :: forall x xs ys zs m.
   ( Monad m
   , MaybePopable x xs
   , Liftable (Filter x xs) zs
   , Liftable ys zs
   ) => Variant xs -> (x -> Flow m ys) -> Flow m zs
{-# INLINE (?~^^>) #-}
(?~^^>) v f = case popVariantMaybe v of
   Right x -> liftVariant <$> f x
   Left ys -> liftVariant <$> return ys

infixl 0 ?~^^>

-- | Pop element, lift the result
(>?~^^>) :: forall x xs ys zs m.
   ( Monad m
   , MaybePopable x xs
   , Liftable (Filter x xs) zs
   , Liftable ys zs
   ) => Flow m xs -> (x -> Flow m ys) -> Flow m zs
{-# INLINE (>?~^^>) #-}
(>?~^^>) = liftm (?~^^>)

infixl 0 >?~^^>

-- | Pop element, lift the result
(%~^^>) :: forall x xs ys zs m.
   ( Monad m
   , Popable x xs
   , Liftable (Filter x xs) zs
   , Liftable ys zs
   ) => Variant xs -> (x -> Flow m ys) -> Flow m zs
{-# INLINE (%~^^>) #-}
(%~^^>) = (?~^^>)

infixl 0 %~^^>

-- | Pop element, lift the result
(>%~^^>) :: forall x xs ys zs m.
   ( Monad m
   , Popable x xs
   , Liftable (Filter x xs) zs
   , Liftable ys zs
   ) => Flow m xs -> (x -> Flow m ys) -> Flow m zs
{-# INLINE (>%~^^>) #-}
(>%~^^>) = liftm (%~^^>)

infixl 0 >%~^^>

-- | Pop element, connect to the expected output
(?~^>) :: forall x xs zs m.
   ( Monad m
   , MaybePopable x xs
   , Liftable (Filter x xs) zs
   ) => Variant xs -> (x -> Flow m zs) -> Flow m zs
{-# INLINE (?~^>) #-}
(?~^>) v f = case popVariantMaybe v of
   Right x -> f x
   Left ys -> return (liftVariant ys)

infixl 0 ?~^>

-- | Pop element, connect to the expected output
(>?~^>) :: forall x xs zs m.
   ( Monad m
   , MaybePopable x xs
   , Liftable (Filter x xs) zs
   ) => Flow m xs -> (x -> Flow m zs) -> Flow m zs
{-# INLINE (>?~^>) #-}
(>?~^>) = liftm (?~^>)

infixl 0 >?~^>

-- | Pop element, connect to the expected output
(%~^>) :: forall x xs zs m.
   ( Monad m
   , Popable x xs
   , Liftable (Filter x xs) zs
   ) => Variant xs -> (x -> Flow m zs) -> Flow m zs
{-# INLINE (%~^>) #-}
(%~^>) = (?~^>)

infixl 0 %~^>

-- | Pop element, connect to the expected output
(>%~^>) :: forall x xs zs m.
   ( Monad m
   , Popable x xs
   , Liftable (Filter x xs) zs
   ) => Flow m xs -> (x -> Flow m zs) -> Flow m zs
{-# INLINE (>%~^>) #-}
(>%~^>) = liftm (%~^>)

infixl 0 >%~^>

-- | Pop element, use the same output type
(?~$>) :: forall x xs m.
   ( Monad m
   , MaybePopable x xs
   ) => Variant xs -> (x -> Flow m xs) -> Flow m xs
{-# INLINE (?~$>) #-}
(?~$>) v f = case popVariantMaybe v of
   Right x -> f x
   Left _  -> return v

infixl 0 ?~$>

-- | Pop element, use the same output type
(>?~$>) :: forall x xs m.
   ( Monad m
   , MaybePopable x xs
   ) => Flow m xs -> (x -> Flow m xs) -> Flow m xs
{-# INLINE (>?~$>) #-}
(>?~$>) = liftm (?~$>)

infixl 0 >?~$>

-- | Pop element, use the same output type
(%~$>) :: forall x xs m.
   ( Monad m
   , Popable x xs
   ) => Variant xs -> (x -> Flow m xs) -> Flow m xs
{-# INLINE (%~$>) #-}
(%~$>) = (?~$>)

infixl 0 %~$>

-- | Pop element, use the same output type
(>%~$>) :: forall x xs m.
   ( Monad m
   , Popable x xs
   ) => Flow m xs -> (x -> Flow m xs) -> Flow m xs
{-# INLINE (>%~$>) #-}
(>%~$>) = liftm (%~$>)

infixl 0 >%~$>

-- | Pop element, fusion the result
(?~|>) :: forall x xs ys zs m.
   ( Monad m
   , MaybePopable x xs
   , Liftable (Filter x xs) zs
   , Liftable ys zs
   , zs ~ Union (Filter x xs) ys
   ) => Variant xs -> (x -> Flow m ys) -> Flow m zs
{-# INLINE (?~|>) #-}
(?~|>) v f = case popVariantMaybe v of
   Right x -> liftVariant <$> f x
   Left ys -> return (liftVariant ys)

infixl 0 ?~|>

-- | Pop element, fusion the result
(>?~|>) :: forall x xs ys zs m.
   ( Monad m
   , MaybePopable x xs
   , Liftable (Filter x xs) zs
   , Liftable ys zs
   , zs ~ Union (Filter x xs) ys
   ) => Flow m xs -> (x -> Flow m ys) -> Flow m zs
{-# INLINE (>?~|>) #-}
(>?~|>) = liftm (?~|>)

infixl 0 >?~|>

-- | Pop element, fusion the result
(%~|>) :: forall x xs ys zs m.
   ( Monad m
   , Popable x xs
   , Liftable (Filter x xs) zs
   , Liftable ys zs
   , zs ~ Union (Filter x xs) ys
   ) => Variant xs -> (x -> Flow m ys) -> Flow m zs
{-# INLINE (%~|>) #-}
(%~|>) = (?~|>)

infixl 0 %~|>

-- | Pop element, fusion the result
(>%~|>) :: forall x xs ys zs m.
   ( Monad m
   , Popable x xs
   , Liftable (Filter x xs) zs
   , Liftable ys zs
   , zs ~ Union (Filter x xs) ys
   ) => Flow m xs -> (x -> Flow m ys) -> Flow m zs
{-# INLINE (>%~|>) #-}
(>%~|>) = liftm (%~|>)

infixl 0 >%~|>

-- | Pop element and perform effect. Passthrough the input value.
(?~=>) :: forall x xs m.
   ( Monad m
   , MaybePopable x xs
   ) => Variant xs -> (x -> m ()) -> Flow m xs
{-# INLINE (?~=>) #-}
(?~=>) v f = case popVariantMaybe v of
   Right x -> f x >> return v
   Left _  -> return v

infixl 0 ?~=>

-- | Pop element and perform effect. Passthrough the input value.
(>?~=>) :: forall x xs m.
   ( Monad m
   , MaybePopable x xs
   ) => Flow m xs -> (x -> m ()) -> Flow m xs
{-# INLINE (>?~=>) #-}
(>?~=>) = liftm (?~=>)

infixl 0 >?~=>

-- | Pop element and perform effect. Passthrough the input value.
(%~=>) :: forall x xs m.
   ( Monad m
   , Popable x xs
   ) => Variant xs -> (x -> m ()) -> Flow m xs
{-# INLINE (%~=>) #-}
(%~=>) = (?~=>)

infixl 0 %~=>

-- | Pop element and perform effect. Passthrough the input value.
(>%~=>) :: forall x xs m.
   ( Monad m
   , Popable x xs
   ) => Flow m xs -> (x -> m ()) -> Flow m xs
{-# INLINE (>%~=>) #-}
(>%~=>) = liftm (%~=>)

infixl 0 >%~=>

-- | Pop element and perform effect.
(?~!>) :: forall x xs m.
   ( Monad m
   , MaybePopable x xs
   ) => Variant xs -> (x -> m ()) -> m ()
{-# INLINE (?~!>) #-}
(?~!>) v f = case popVariantMaybe v of
   Right x -> f x
   Left _  -> return ()

infixl 0 ?~!>

-- | Pop element and perform effect.
(>?~!>) :: forall x xs m.
   ( Monad m
   , MaybePopable x xs
   ) => Flow m xs -> (x -> m ()) -> m ()
{-# INLINE (>?~!>) #-}
(>?~!>) = liftm (?~!>)

infixl 0 >?~!>

-- | Pop element and perform effect.
(%~!>) :: forall x xs m.
   ( Monad m
   , Popable x xs
   ) => Variant xs -> (x -> m ()) -> m ()
{-# INLINE (%~!>) #-}
(%~!>) = (?~!>)

infixl 0 %~!>

-- | Pop element and perform effect.
(>%~!>) :: forall x xs m.
   ( Monad m
   , Popable x xs
   ) => Flow m xs -> (x -> m ()) -> m ()
{-# INLINE (>%~!>) #-}
(>%~!>) = liftm (%~!>)

infixl 0 >%~!>

-- | Pop element and perform effect.
(?~!!>) :: forall x xs m.
   ( Monad m
   , MaybePopable x xs
   ) => Variant xs -> (x -> m ()) -> Flow m (Filter x xs)
{-# INLINE (?~!!>) #-}
(?~!!>) v f = case popVariantMaybe v of
   Right x -> f x >> error "?~!!> error"
   Left u  -> return u

infixl 0 ?~!!>

-- | Pop element and perform effect.
(>?~!!>) :: forall x xs m.
   ( Monad m
   , MaybePopable x xs
   ) => Flow m xs -> (x -> m ()) -> Flow m (Filter x xs)
{-# INLINE (>?~!!>) #-}
(>?~!!>) = liftm (?~!!>)

infixl 0 >?~!!>

-- | Pop element and perform effect.
(%~!!>) :: forall x xs m.
   ( Monad m
   , Popable x xs
   ) => Variant xs -> (x -> m ()) -> Flow m (Filter x xs)
{-# INLINE (%~!!>) #-}
(%~!!>) = (?~!!>)

infixl 0 %~!!>

-- | Pop element and perform effect.
(>%~!!>) :: forall x xs m.
   ( Monad m
   , Popable x xs
   ) => Flow m xs -> (x -> m ()) -> Flow m (Filter x xs)
{-# INLINE (>%~!!>) #-}
(>%~!!>) = liftm (%~!!>)

infixl 0 >%~!!>

--------------------------------------------------------------
-- Helpers
--------------------------------------------------------------


-- | Make a flow operator
makeFlowOp :: Monad m =>
      (Variant as -> Either (Variant bs) (Variant cs))
      -> (Variant cs -> Flow m ds)
      -> (Either (Variant bs) (Variant ds) -> es)
      -> Variant as -> m es
{-# INLINE makeFlowOp #-}
makeFlowOp select apply combine v = combine <$> traverse apply (select v)

-- | Make a flow operator
makeFlowOpM :: Monad m =>
      (Variant as -> Either (Variant bs) (Variant cs))
      -> (Variant cs -> Flow m ds)
      -> (Either (Variant bs) (Variant ds) -> es)
      -> Flow m as -> m es
{-# INLINE makeFlowOpM #-}
makeFlowOpM select apply combine v = v >>= makeFlowOp select apply combine


-- | Select the first value
selectFirst :: Variant (x ': xs) -> Either (Variant xs) (Variant '[x])
{-# INLINE selectFirst #-}
selectFirst = fmap (toVariantAt @0) . popVariantHead

-- | Select the tail
selectTail :: Variant (x ': xs) -> Either (Variant '[x]) (Variant xs)
{-# INLINE selectTail #-}
selectTail = flipEither . selectFirst
   where
      flipEither (Left x)  = Right x
      flipEither (Right x) = Left x

-- | Select by type
selectType ::
   ( Popable x xs
   ) => Variant xs -> Either (Variant (Filter x xs)) (Variant '[x])
{-# INLINE selectType #-}
selectType = fmap (toVariantAt @0) . popVariant

-- | Const application
applyConst :: Flow m ys -> (Variant xs -> Flow m ys)
{-# INLINE applyConst #-}
applyConst = const

-- | Pure application
applyPure :: Monad m => (Variant xs -> Variant ys) -> Variant xs -> Flow m ys
{-# INLINE applyPure #-}
applyPure f = return . f

-- | Lift a monadic function
applyM :: Monad m => (a -> m b) -> Variant '[a] -> Flow m '[b]
{-# INLINE applyM #-}
applyM = liftF

-- | Lift a monadic function
applyVM :: Monad m => (Variant a -> m b) -> Variant a -> Flow m '[b]
{-# INLINE applyVM #-}
applyVM f = fmap (toVariantAt @0) . f

-- | Lift a monadic function
applyF :: (a -> Flow m b) -> Variant '[a] -> Flow m b
{-# INLINE applyF #-}
applyF f = f . variantToValue

-- | Set the first value (the "correct" one)
combineFirst :: forall x xs. Either (Variant xs) (Variant '[x]) -> Variant (x ': xs)
{-# INLINE combineFirst #-}
combineFirst = \case
   Right x -> appendVariant  @xs x
   Left xs -> prependVariant @'[x] xs

-- | Set the first value, keep the same tail type 
combineSameTail :: forall x xs.
   Either (Variant xs) (Variant (x ': xs)) -> Variant (x ': xs)
{-# INLINE combineSameTail #-}
combineSameTail = \case
   Right x -> x
   Left xs -> prependVariant @'[x] xs

-- | Return the valid variant unmodified
combineEither :: Either (Variant xs) (Variant xs) -> Variant xs
{-# INLINE combineEither #-}
combineEither = \case
   Right x -> x
   Left x  -> x

-- | Concatenate unselected values
combineConcat :: forall xs ys.
   ( KnownNat (Length xs)
   ) => Either (Variant ys) (Variant xs) -> Variant (Concat xs ys)
{-# INLINE combineConcat #-}
combineConcat = \case
   Right xs -> appendVariant  @ys xs
   Left ys  -> prependVariant @xs ys

-- | Union
combineUnion ::
   ( Liftable xs (Union xs ys)
   , Liftable ys (Union xs ys)
   ) => Either (Variant ys) (Variant xs) -> Variant (Union xs ys)
{-# INLINE combineUnion #-}
combineUnion = \case
   Right xs -> liftVariant xs
   Left  ys -> liftVariant ys

-- | Lift unselected
combineLiftUnselected ::
   ( Liftable ys xs
   ) => Either (Variant ys) (Variant xs) -> Variant xs
{-# INLINE combineLiftUnselected #-}
combineLiftUnselected = \case
   Right xs -> xs
   Left ys  -> liftVariant ys

-- | Lift both
combineLiftBoth ::
   ( Liftable ys zs
   , Liftable xs zs
   ) => Either (Variant ys) (Variant xs) -> Variant zs
{-# INLINE combineLiftBoth #-}
combineLiftBoth = \case
   Right xs -> liftVariant xs
   Left ys  -> liftVariant ys

-- | Single value
combineSingle :: Either (Variant '[x]) (Variant '[x]) -> x
{-# INLINE combineSingle #-}
combineSingle = \case
   Right x -> variantToValue x
   Left  x -> variantToValue x


-- | Lift a pure function into a Variant to Variant function
liftV :: (a -> b) -> Variant '[a] -> Variant '[b]
liftV = updateVariantAt @0

-- | Lift a function into a Flow
liftF :: Monad m => (a -> m b) -> Variant '[a] -> Flow m '[b]
liftF = updateVariantFirstM @0


-----------------------------------
-- Operation on every element
-----------------------------------

-- | Replace the RHS of every function type in the list with `v`
type family ReplaceRHS f v where
   ReplaceRHS '[] _              = '[]
   ReplaceRHS ((x -> _) ': xs) v = (x -> v) ': ReplaceRHS xs v

-- | Extract the RHS of every function type in the list
type family ExtractRHS f where
   ExtractRHS '[]              = '[]
   ExtractRHS ((_ -> x) ': xs) = x ': ExtractRHS xs

type LiftContTuple x = ListToTuple (ReplaceRHS (TupleToList x) (Variant (ExtractRHS (TupleToList x))))

class LiftCont x where
   -- | Lift a tuple of functions (a -> r1, b -> r2, ...) into a tuple of
   -- functions (a -> V '[r1,r2,...], b -> V '[r1,r2,...], ...)
   liftCont :: x -> LiftContTuple x

instance LiftCont (Single (a -> b)) where
   liftCont (Single a) = Single (V . a)

instance LiftCont (a->b,c->d) where
   liftCont (a,b) =
      ( toVariantAt @0 . a
      , toVariantAt @1 . b
      )

instance LiftCont (a->b,c->d,e->f) where
   liftCont (a,b,c) =
      ( toVariantAt @0 . a
      , toVariantAt @1 . b
      , toVariantAt @2 . c
      )

instance LiftCont (a->b,c->d,e->f,g->h) where
   liftCont (a,b,c,d) =
      ( toVariantAt @0 . a
      , toVariantAt @1 . b
      , toVariantAt @2 . c
      , toVariantAt @3 . d
      )

instance LiftCont (a->b,c->d,e->f,g->h,i->j) where
   liftCont (a,b,c,d,e) =
      ( toVariantAt @0 . a
      , toVariantAt @1 . b
      , toVariantAt @2 . c
      , toVariantAt @3 . d
      , toVariantAt @4 . e
      )

instance LiftCont (a->b,c->d,e->f,g->h,i->j,k->l) where
   liftCont (a,b,c,d,e,f) =
      ( toVariantAt @0 . a
      , toVariantAt @1 . b
      , toVariantAt @2 . c
      , toVariantAt @3 . d
      , toVariantAt @4 . e
      , toVariantAt @5 . f
      )

instance LiftCont (a->b,c->d,e->f,g->h,i->j,k->l,m->n) where
   liftCont (a,b,c,d,e,f,g) =
      ( toVariantAt @0 . a
      , toVariantAt @1 . b
      , toVariantAt @2 . c
      , toVariantAt @3 . d
      , toVariantAt @4 . e
      , toVariantAt @5 . f
      , toVariantAt @6 . g
      )

instance LiftCont (a->b,c->d,e->f,g->h,i->j,k->l,m->n,o->p) where
   liftCont (a,b,c,d,e,f,g,h) =
      ( toVariantAt @0 . a
      , toVariantAt @1 . b
      , toVariantAt @2 . c
      , toVariantAt @3 . d
      , toVariantAt @4 . e
      , toVariantAt @5 . f
      , toVariantAt @6 . g
      , toVariantAt @7 . h
      )

instance LiftCont (a->b,c->d,e->f,g->h,i->j,k->l,m->n,o->p,q->r) where
   liftCont (a,b,c,d,e,f,g,h,i) =
      ( toVariantAt @0 . a
      , toVariantAt @1 . b
      , toVariantAt @2 . c
      , toVariantAt @3 . d
      , toVariantAt @4 . e
      , toVariantAt @5 . f
      , toVariantAt @6 . g
      , toVariantAt @7 . h
      , toVariantAt @8 . i
      )

-- | Pure multi-map
--
-- Map functions on a variant and produce a resulting variant
--
-- @
--     > (V 'c' :: V '[Char,String]) -|| (ord,map toUpper)
--     V 99 :: V '[Int,String]
--
--     > (V "test" :: V '[Char,String]) -|| (ord,map toUpper)
--     V "TEST" :: V '[Int,String]
--
--     > (V "test" :: V '[Char,String]) -|| (ord,length)
--     V 4 :: V '[Int,Int]
-- @
--
(-||) :: forall fs xs zs.
   ( LiftCont fs
   , zs ~ ExtractRHS (TupleToList fs)
   , LiftContTuple fs ~ ContListToTuple xs (Variant zs)
   , ContVariant xs
   ) => Variant xs -> fs -> Variant zs
(-||) v fs = variantToCont v >::> liftCont fs

-- | Applicative pure multi-map
(-||>) :: forall m fs xs zs ks.
   ( LiftCont fs
   , zs ~ ExtractRHS (TupleToList fs)
   , LiftContTuple fs ~ ContListToTuple xs (Variant zs)
   , ContVariant xs
   , ks ~ ExtractMonad m zs
   , Applicative m
   ) => Variant xs -> fs -> Flow m ks
(-||>) v fs = joinVariant (v -|| fs)

-- | Monadic pure multi-map
(>-||>) :: forall m fs xs zs ks.
   ( LiftCont fs
   , zs ~ ExtractRHS (TupleToList fs)
   , LiftContTuple fs ~ ContListToTuple xs (Variant zs)
   , ContVariant xs
   , ks ~ ExtractMonad m zs
   , Monad m
   ) => Flow m xs -> fs -> Flow m ks
(>-||>) act fs = do
   r <- act
   r -||> fs

-- | Variant multi-map
--
-- Map functions returning a variant on a variant and produce a resulting
-- flattened and nub'ed variant
--
-- @
--     mapInt64 :: Int64 -> V '[Int16,Int32,Int64]
--     mapInt64 x
--        | x <= 0xffff     = toVariantAt @0 (fromIntegral x)
--        | x <= 0xffffffff = toVariantAt @1 (fromIntegral x)
--        | otherwise       = toVariantAt @2 x
--     
--     mapInt32 :: Int32 -> V '[Int16,Int32]
--     mapInt32 x
--        | x <= 0xffff     = toVariantAt @0 (fromIntegral x)
--        | otherwise       = toVariantAt @1 x
--     
--     > V @Int64 @'[Int64,Int32] 10 ~|| (mapInt64,mapInt32)
--     V 10 :: Variant '[Int16, Int32, Int64]
-- @
--
(~||) :: forall fs xs zs ys rs.
   ( LiftCont fs
   , zs ~ ExtractRHS (TupleToList fs)
   , LiftContTuple fs ~ ContListToTuple xs (Variant zs)
   , ContVariant xs
   , ys ~ FlattenVariant zs
   , Flattenable (Variant zs) (Variant ys)
   , Liftable ys (Nub ys)
   , rs ~ Nub ys
   ) => Variant xs -> fs -> Variant rs
(~||) v fs = nubVariant (flattenVariant (v -|| fs))

-- | Applicative variant multi-map
--
-- @
--    mapInt64 :: Int64 -> IO (V '[Int16,Int32,Int64])
--    mapInt64 x
--       | x <= 0xffff     = do
--          putStrLn "Found Int16!"
--          return (toVariantAt @0 (fromIntegral x))
--       | x <= 0xffffffff = do
--          putStrLn "Found Int32!"
--          return (toVariantAt @1 (fromIntegral x))
--       | otherwise       = do
--          putStrLn "Found Int64!"
--          return (toVariantAt @2 x)
--
--    mapInt32 :: Int32 -> IO (V '[Int16,Int32])
--    mapInt32 x
--       | x <= 0xffff     = do
--          putStrLn "Found Int16!"
--          return (toVariantAt @0 (fromIntegral x))
--       | otherwise       = do
--          putStrLn "Found Int32!"
--          return (toVariantAt @1 x)
--
--    v = V @Int64 @'[Int64,Int32] 10
--
--    > x <- v -||> (mapInt64,mapInt32)
--    Found Int16!
--
--    > :t x
--    x :: V '[V '[Int16, Int32, Int64], V '[Int16, Int32]]
--
--    > x <- v ~||> (mapInt64,mapInt32)
--    Found Int16!
--
--    > :t x
--    x :: V '[Int16, Int32, Int64]
-- @
--
(~||>) :: forall m fs xs zs ks ys rs.
   ( ContVariant xs
   , LiftCont fs
   , zs ~ ExtractRHS (TupleToList fs)
   , LiftContTuple fs ~ ContListToTuple xs (Variant zs)
   , ks ~ ExtractMonad m zs
   , ys ~ FlattenVariant ks
   , Flattenable (Variant ks) (Variant ys)
   , rs ~ Nub ys
   , Liftable ys rs
   , Applicative m
   ) => Variant xs -> fs -> Flow m rs
(~||>) v fs = nubVariant <$> (flattenVariant <$> joinVariant (v -|| fs))

-- | Monadic variant multi-map
(>~||>) :: forall m fs xs zs ks ys rs.
   ( ContVariant xs
   , LiftCont fs
   , zs ~ ExtractRHS (TupleToList fs)
   , LiftContTuple fs ~ ContListToTuple xs (Variant zs)
   , ks ~ ExtractMonad m zs
   , ys ~ FlattenVariant ks
   , Flattenable (Variant ks) (Variant ys)
   , rs ~ Nub ys
   , Liftable ys rs
   , Monad m
   ) => Flow m xs -> fs -> Flow m rs
(>~||>) act fs = do
   r <- act
   r ~||> fs
