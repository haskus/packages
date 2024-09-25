module Haskus.Arch.X86_64.ISA.Encoding.Segment
  ( Segment (..)
  , segmentOverridePrefix
  )
where

import Haskus.Arch.X86_64.ISA.Encoding.Prefix

-- | Memory segment
data Segment
  = CS
  | DS
  | ES
  | FS
  | GS
  | SS
  deriving (Show,Eq,Ord)

-- | Get prefix to override memory operand segment
segmentOverridePrefix :: Segment -> Prefix
segmentOverridePrefix = \case
  CS -> P_2E
  DS -> P_3E
  ES -> P_26
  FS -> P_64
  GS -> P_65
  SS -> P_36
