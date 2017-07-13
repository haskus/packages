{-# LANGUAGE LambdaCase #-}

-- | Utils for Maybe data type
module Haskus.Utils.Maybe
   ( onNothing
   , onNothingM
   , fromMaybeM
   , headMaybe
   , module Data.Maybe
   )
where

import Data.Maybe

-- | Flipped `fromMaybe`
onNothing :: Maybe a -> a -> a
onNothing = flip fromMaybe

-- | Flipped `fromMaybeM`
onNothingM :: Monad m => m (Maybe a) -> m a -> m a
onNothingM = flip fromMaybeM

-- | fromMaybe in a Monad
fromMaybeM :: Monad m => m a -> m (Maybe a) -> m a
fromMaybeM v f = f >>= \case
   Nothing -> v
   Just x  -> return x

-- | Get the head of the list if the latter is not empty
headMaybe :: [a] -> Maybe a
headMaybe []    = Nothing
headMaybe (x:_) = Just x
