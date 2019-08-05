{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

-- | Samplers
module Haskus.UI.Sampler
   ( Sample
   , Sampler
   , SamplerT
   , SamplerState (..)
   , initSamplerState
   , evalSampler
   , evalSamplerT
   , getRandom
   , splitRandom
   , shuffleList
   , shuffleListN
   , centerSquareSample
   -- * Blenders
   , affineColors
   , weighColors
   , constantBlender
   , linearBlender
   -- * Unit square samplers
   , shuffleSampler
   , singleSampler'
   , singleSampler
   , regularSquareSampler'
   , regularSquareSampler
   , randomSquareSampler'
   , randomSquareSampler
   , jitteredSquareSampler'
   , jitteredSquareSampler
   , nRooksSquareSampler'
   , nRooksSquareSampler
   , multiJitteredSquareSampler'
   , multiJitteredSquareSampler
   , hammersleySquareSampler'
   , hammersleySquareSampler
   )
where

import System.Random.TF.Gen
import System.Random.Shuffle
import Control.Monad.Trans.State
import Data.Bifunctor

import Haskus.Utils.Flow
import Haskus.UI.Common
import Haskus.UI.Color
import Haskus.UI.Maths.Linear
import Haskus.Number.Word
import Haskus.Binary.Bits

newtype SamplerState = SamplerState
   { samplerGen :: TFGen
   }

initSamplerState :: SamplerState
initSamplerState = SamplerState
   { samplerGen = seedTFGen (0x0123,0x4567,0x89AB,0xCDEF)
   }

type RawGenType = Word32

type Sampler    = State SamplerState
type SamplerT m = StateT SamplerState m
type Sample     = V2 Dist

-- | Evaluate a sampler
evalSamplerT :: Monad m => SamplerT m a -> m a
evalSamplerT s = evalStateT s initSamplerState

-- | Evaluate a sampler
evalSampler :: Sampler a -> a
evalSampler s = evalState s initSamplerState

-- | Return a number between 0.0 and 1.0
getRandom :: Monad m => SamplerT m Dist
getRandom = do
   s <- get
   let
      (n,nextGen) = next (samplerGen s)

   put <| s { samplerGen = nextGen }

   let
      -- specialized fromIntegral to ensure that we correctly compute the
      -- maxBound of RawGenType
      fi :: RawGenType -> Dist
      fi = fromIntegral
   return (fi n / fi maxBound)

-- | Split the random number generator: store one, return the other
splitRandom :: Monad m => SamplerT m TFGen
splitRandom = do
   s <- get
   let
      (one,other) = split (samplerGen s)
   put <| s { samplerGen = one }
   return other

-- | Shuffle a list into a split PRNG
shuffleList :: Monad m => [a] -> SamplerT m [a]
shuffleList xs = shuffleListN xs (length xs)

-- | Shuffle a list into a split PRNG (specify the lenth of the list)
shuffleListN :: Monad m => [a] -> Int -> SamplerT m [a]
shuffleListN xs n = do
   g <- splitRandom
   return (shuffle' xs n g)


-- | Blend sample colors with a constant factors (1/n for all samples)
constantBlender :: [(Sample,Color)] -> Color
constantBlender cs = affineColors (repeat sampleWeigh `zip` fmap snd cs)
   where
      n           = fromIntegral (length cs)
      sampleWeigh = 1.0 / n

-- | Blend sample colors with their distance to the center as a factor
linearBlender :: [(Sample,Color)] -> Color
linearBlender cs = weighColors <| fmap (first dist) cs
   where
      orig        = V2 0.0 0.0
      dist x      = realToFrac (distance orig x)

-- | Blend colors with a weigh factor between 0.0 and 1.0.
-- The sum of the factors must be 1.0
affineColors :: [(ColorUnit,Color)] -> Color
affineColors cs = affineCombo (tail cs) (snd (head cs))

-- | Blend colors with a weigh factor. The factors are automatically ajusted to
-- be in the range (0.0,1.0) and so that their sum is equal to 1.0
weighColors :: [(ColorUnit,Color)] -> Color
weighColors cs = affineColors <| fmap (first (/ weighSum)) cs
   where
      weighSum = sum (fmap fst cs)

----------------------------------------------------------------------
-- Sampler
----------------------------------------------------------------------

-- | Center a square sample between (0,0) and (1,1) to make it between
-- (-0.5,-0.5) and (0.5,0.5).
centerSquareSample :: Sample -> Sample
centerSquareSample (V2 x y) = V2 (x-0.5) (y-0.5)

-- | Shuffle the samples
shuffleSampler :: Monad m => SamplerT m [a] -> SamplerT m [a]
shuffleSampler m = do
   xs <- m
   shuffleList xs

-- | Single sample (0.5,0.5)
singleSampler' :: Monad m => SamplerT m [Sample]
singleSampler' = return [V2 0.5 0.5]

-- | Single centered sample (0.0,0.0)
singleSampler :: Monad m => SamplerT m [Sample]
singleSampler = return [V2 0.0 0.0]

-- | Return a regular grid of n*n samples on the unit square (0.0,1.0)
regularSquareSampler' :: Monad m => Word -> SamplerT m [Sample]
regularSquareSampler' n = do
   let
      d  = 1.0 / fromIntegral n
      d2 = 0.5 / fromIntegral n
      xs = [d2 + fromIntegral k*d | k <- [0..n-1]]
   return [ V2 x y
          | x <- xs
          , y <- xs
          ]

-- | Return a regular grid of n*n samples centered on the unit square
-- (-0.5,+0.5)
regularSquareSampler :: Monad m => Word -> SamplerT m [Sample]
regularSquareSampler n = fmap centerSquareSample <|| regularSquareSampler' n

-- | Return a random set of n*n samples on the unit square (0.0,1.0)
randomSquareSampler' :: Monad m => Word -> SamplerT m [Sample]
randomSquareSampler' n = replicateM (fromIntegral (n*n)) <| do
   x <- getRandom
   y <- getRandom
   return <| V2 x y

-- | Return a random set of n*n samples centered on the unit square
-- (-0.5,+0.5)
randomSquareSampler :: Monad m => Word -> SamplerT m [Sample]
randomSquareSampler n = fmap centerSquareSample <|| randomSquareSampler' n

-- | Return a random set of n*n samples on the unit square (0.0,1.0) using
-- jittered (stratified) sampling. I.e. we divide the unit square into n*n equal
-- non-overlapping/non-gaping domains (each domain is called a /stratum/) and
-- then we randomly place one sample per domain.
jitteredSquareSampler' :: Monad m => Word -> SamplerT m [Sample]
jitteredSquareSampler' n = do
   let
      n' = fromIntegral n
      d  = 1.0 / n'
      xs = (,) <$> [0..n-1] <*> [0..n-1]
   forM xs <| \(px :: Word,py) -> do
      x <- (/n') <$> getRandom
      y <- (/n') <$> getRandom
      return <| V2 (fromIntegral px*d+x) (fromIntegral py*d+y)

-- | Return a random set of n*n samples on the centered unit square (-0.5,+0.5)
-- using jittered (stratified) sampling. I.e. we divide the unit square into n*n
-- equal non-overlapping/non-gaping domains (each domain is called a /stratum/)
-- and then we randomly place one sample per domain.
jitteredSquareSampler :: Monad m => Word -> SamplerT m [Sample]
jitteredSquareSampler n = fmap centerSquareSample <|| jitteredSquareSampler' n


-- | Return a random set of n samples on the unit square (0.0,1.0) using
-- n-rooks sampling. I.e. we place one random sample per column and per row in a
-- n*n grid.
nRooksSquareSampler' :: Monad m => Word -> SamplerT m [Sample]
nRooksSquareSampler' n = do
   let
      n' = fromIntegral n
      d  = 1.0 / n'
   samples <- forM [0..n-1] <| \(px :: Word) -> do
      x <- (/n') <$> getRandom
      y <- (/n') <$> getRandom
      return <| V2 (fromIntegral px*d+x) (fromIntegral px*d+y)
   shuffleListN samples (fromIntegral n)

-- | Return a random set of n*n samples on the centered unit square (-0.5,+0.5)
-- using jittered (stratified) sampling. I.e. we divide the unit square into n*n
-- equal non-overlapping/non-gaping domains (each domain is called a /stratum/)
-- and then we randomly place one sample per domain.
nRooksSquareSampler :: Monad m => Word -> SamplerT m [Sample]
nRooksSquareSampler n = fmap centerSquareSample <|| nRooksSquareSampler' n

-- | Return a random set of n*n samples on the unit square (0.0,1.0) using
-- multi-jittered sampling. I.e. jittered sampling with n-rooks sampling in each
-- stratum.
multiJitteredSquareSampler' :: Monad m => Word -> SamplerT m [Sample]
multiJitteredSquareSampler' n = do
   let
      n' = fromIntegral n
      n2 = fromIntegral (n*n)
      dn2 = 1.0 / n2
      dn  = 1.0 / n'
      xs  = [ (fromIntegral x, fromIntegral y)
            | x <- [0..n-1]
            , y <- [0..n-1]
            ]

   -- shuffle the inner indices
   rxs <- shuffleListN [0..n-1] (fromIntegral n)
   rys <- shuffleListN [0..n-1] (fromIntegral n)
   let
      rs  = [ (fromIntegral x, fromIntegral y)
            | x <- rxs
            , y <- rys
            ]

   forM (xs `zip` rs) <| \((ox,oy),(ix,iy)) -> do
      x <- (/n2) <$> getRandom
      y <- (/n2) <$> getRandom
      return <| V2 (ox*dn + iy*dn2 + x) (oy*dn + ix*dn2 + y)

-- | Return a random set of n*n samples on the centered unit square (-0.5,+0.5)
-- using multi-jittered sampling. I.e. jittered sampling with n-rooks sampling
-- in each stratum.
multiJitteredSquareSampler :: Monad m => Word -> SamplerT m [Sample]
multiJitteredSquareSampler n = fmap centerSquareSample <|| multiJitteredSquareSampler' n


-- | Return a random set of n samples on the unit square (0.0,1.0) using
-- Hammersley sampling on base 2.
hammersleySquareSampler' :: Monad m => Word -> SamplerT m [Sample]
hammersleySquareSampler' n = do
   let
      phi' !x !_f 0 = x
      phi' !x !f j  = phi' (x + f*(fromIntegral (j .&. 1))) (f*0.5) (j `shiftR` 1)
      phi j         = phi' 0.0 0.5 j

   return <| [ V2 (fromIntegral i / fromIntegral n) (phi i)
             | i <- [0..n-1]
             ]

-- | Return a random set of n samples on the centered unit square (-0.5,+0.5) using
-- Hammersley sampling on base 2.
hammersleySquareSampler :: Monad m => Word -> SamplerT m [Sample]
hammersleySquareSampler n = fmap centerSquareSample <|| multiJitteredSquareSampler' n
