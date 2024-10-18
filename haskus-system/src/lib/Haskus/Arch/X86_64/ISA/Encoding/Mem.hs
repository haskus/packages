module Haskus.Arch.X86_64.ISA.Encoding.Mem
  ( encodeMem
  , encodeMem16
  , encodeMem32
  , encodeMem64
  , encodeMemRel
  , encode_mem16
  , encode_mem32
  , encode_mem64
  , Mem (..)
  , emptyMem
  , MemAddr (..)
  , MemLock(..)
  , isMemAny
  , isMem8
  , isMem16
  , isMem32
  , isMem64
  , isMem128
  , isMem256
  )
where

import Prelude hiding (mod)
import Haskus.Arch.X86_64.ISA.Encoding.Disp
import Haskus.Arch.X86_64.ISA.Encoding.Reg
import Haskus.Arch.X86_64.ISA.Encoding.SIB
import Haskus.Arch.X86_64.ISA.Encoding.Rex
import Haskus.Arch.X86_64.ISA.Encoding.ModRM
import Haskus.Arch.X86_64.ISA.Encoding.Enc
import Haskus.Arch.X86_64.ISA.Encoding.Segment
import Haskus.Arch.X86_64.ISA.Encoding.Prefix
import Haskus.Arch.X86_64.ISA.Size
import Haskus.Arch.X86_64.ISA.Context
import Haskus.Binary.Cast

import Data.List (intercalate)
import Haskus.Utils.Maybe (catMaybes)

encodeMem16 :: Maybe Reg -> Maybe Reg -> Disp -> Maybe (ModRM,Disp)
encodeMem16 base index disp
  -- base case
  | Just r <- encode_mem16 base index disp
  = Just r

  -- try switching the arguments if neither register is BP, otherwise the default
  -- segment would switch from DS to SS or vice-versa
  | index /= JR_BP && base /= JR_BP
  , Just r <- encode_mem16 index base disp
  = Just r

  -- if disp8, retry with it sign-extended to disp16
  | Disp8 d <- disp
  = encodeMem16 base index (Disp16 (i16FromI8 d))

  | otherwise
  = Nothing


-- | Return (modrm,disp) for the given 16-bit addressing form base-index-disp.
--
-- modrm.reg is set to 0
--
-- This function doesn't try to arrange its arguments differently to make them
-- encodable. Use encodeMem16 for this
encode_mem16 :: Maybe Reg -> Maybe Reg -> Disp -> Maybe (ModRM,Disp)
encode_mem16 = \cases
  JR_BX   JR_SI   NoDisp      -> r 0b00 0b000 NoDisp
  JR_BX   JR_DI   NoDisp      -> r 0b00 0b001 NoDisp
  JR_BP   JR_SI   NoDisp      -> r 0b00 0b010 NoDisp
  JR_BP   JR_DI   NoDisp      -> r 0b00 0b011 NoDisp
  JR_SI   Nothing NoDisp      -> r 0b00 0b100 NoDisp
  JR_DI   Nothing NoDisp      -> r 0b00 0b101 NoDisp
  Nothing Nothing (Disp16 d)  -> r 0b00 0b110 (Disp16 d)
  JR_BX   Nothing NoDisp      -> r 0b00 0b111 NoDisp

  JR_BX   JR_SI   (Disp8 d)   -> r 0b01 0b000 (Disp8 d)
  JR_BX   JR_DI   (Disp8 d)   -> r 0b01 0b001 (Disp8 d)
  JR_BP   JR_SI   (Disp8 d)   -> r 0b01 0b010 (Disp8 d)
  JR_BP   JR_DI   (Disp8 d)   -> r 0b01 0b011 (Disp8 d)
  JR_SI   Nothing (Disp8 d)   -> r 0b01 0b100 (Disp8 d)
  JR_DI   Nothing (Disp8 d)   -> r 0b01 0b101 (Disp8 d)
  JR_BP   Nothing (Disp8 d)   -> r 0b01 0b110 (Disp8 d)
  JR_BX   Nothing (Disp8 d)   -> r 0b01 0b111 (Disp8 d)

  JR_BX   JR_SI   (Disp16 d)  -> r 0b10 0b000 (Disp16 d)
  JR_BX   JR_DI   (Disp16 d)  -> r 0b10 0b001 (Disp16 d)
  JR_BP   JR_SI   (Disp16 d)  -> r 0b10 0b010 (Disp16 d)
  JR_BP   JR_DI   (Disp16 d)  -> r 0b10 0b011 (Disp16 d)
  JR_SI   Nothing (Disp16 d)  -> r 0b10 0b100 (Disp16 d)
  JR_DI   Nothing (Disp16 d)  -> r 0b10 0b101 (Disp16 d)
  JR_BP   Nothing (Disp16 d)  -> r 0b10 0b110 (Disp16 d)
  JR_BX   Nothing (Disp16 d)  -> r 0b10 0b111 (Disp16 d)

  _       _       _           -> Nothing
  where
    r a b c = Just (mkModRM_mod_rm a b,c)


-- | Return (modrm,disp,sib) for the given 32-bit addressing form scale-index-base-disp
--
encodeMem32 :: Scale -> Maybe Reg -> Maybe Reg -> Disp -> Maybe (ModRM,Disp,Maybe SIB)
encodeMem32 scale index base disp
  -- 2-scale without base and disp: transform 2*r ==> r+r to avoid requiring a 0 disp
  -- only if r /= ESP/EBP
  | Scale2 <- scale
  , Nothing <- base
  , NoDisp <- disp
  , index /= Nothing && index /= JR_ESP && index /= JR_EBP
  = encodeMem32 Scale1 index index NoDisp

  -- base case
  | Just r <- encode_mem32 scale index base disp
  = Just r

  -- 1-scale without base: try using the index as base (if not ESP/EBP)
  | Scale1 <- scale
  , Nothing <- base
  , index /= Nothing && index /= JR_ESP && index /= JR_EBP
  = encodeMem32 Scale1 Nothing index disp

  -- try to sign-extend disp16
  | Disp16 d <- disp
  = encodeMem32 scale index base (Disp32 (i32FromI16 d))

  -- try to sign-extend disp8
  | Disp8 d <- disp
  = encodeMem32 scale index base (Disp32 (i32FromI8 d))

  -- try with a 0 disp
  | NoDisp <- disp
  = encodeMem32 scale index base (Disp8 0)

  | otherwise
  = Nothing


-- | Return (modrm,disp,sib) for the given 32-bit addressing form scale-index-base-disp
--
-- modrm.reg is set to 0
--
-- This function doesn't try to arrange its arguments differently to make them
-- encodable. Use encodeMem32 for this
encode_mem32 :: Scale -> Maybe Reg -> Maybe Reg -> Disp -> Maybe (ModRM,Disp,Maybe SIB)
encode_mem32 = \cases
  -- without SIB byte
  _      Nothing JR_EAX  NoDisp      -> ret 0b00 0b000 NoDisp     Nothing
  _      Nothing JR_ECX  NoDisp      -> ret 0b00 0b001 NoDisp     Nothing
  _      Nothing JR_EDX  NoDisp      -> ret 0b00 0b010 NoDisp     Nothing
  _      Nothing JR_EBX  NoDisp      -> ret 0b00 0b011 NoDisp     Nothing
  _      Nothing Nothing (Disp32 d)  -> ret 0b00 0b101 (Disp32 d) Nothing
  _      Nothing JR_ESI  NoDisp      -> ret 0b00 0b110 NoDisp     Nothing
  _      Nothing JR_EDI  NoDisp      -> ret 0b00 0b111 NoDisp     Nothing

  _      Nothing JR_EAX  (Disp8 d)   -> ret 0b01 0b000 (Disp8 d)  Nothing
  _      Nothing JR_ECX  (Disp8 d)   -> ret 0b01 0b001 (Disp8 d)  Nothing
  _      Nothing JR_EDX  (Disp8 d)   -> ret 0b01 0b010 (Disp8 d)  Nothing
  _      Nothing JR_EBX  (Disp8 d)   -> ret 0b01 0b011 (Disp8 d)  Nothing
  _      Nothing JR_EBP  (Disp8 d)   -> ret 0b01 0b101 (Disp8 d)  Nothing
  _      Nothing JR_ESI  (Disp8 d)   -> ret 0b01 0b110 (Disp8 d)  Nothing
  _      Nothing JR_EDI  (Disp8 d)   -> ret 0b01 0b111 (Disp8 d)  Nothing

  _      Nothing JR_EAX  (Disp32 d)  -> ret 0b10 0b000 (Disp32 d) Nothing
  _      Nothing JR_ECX  (Disp32 d)  -> ret 0b10 0b001 (Disp32 d) Nothing
  _      Nothing JR_EDX  (Disp32 d)  -> ret 0b10 0b010 (Disp32 d) Nothing
  _      Nothing JR_EBX  (Disp32 d)  -> ret 0b10 0b011 (Disp32 d) Nothing
  _      Nothing JR_EBP  (Disp32 d)  -> ret 0b10 0b101 (Disp32 d) Nothing
  _      Nothing JR_ESI  (Disp32 d)  -> ret 0b10 0b110 (Disp32 d) Nothing
  _      Nothing JR_EDI  (Disp32 d)  -> ret 0b10 0b111 (Disp32 d) Nothing

  -- with SIB byte
  scale (scaled_index -> Just index) Nothing (Disp32 d)
    -> ret_sib 0b00 (Disp32 d) (s scale index 0b101)

  scale (scaled_index -> Just index) (sib_base_without_disp -> Just base) NoDisp
    -> ret_sib 0b00 NoDisp (s scale index base)

  scale (scaled_index -> Just index) (sib_base_with_disp -> Just base) (Disp8 d)
    -> ret_sib 0b01 (Disp8 d) (s scale index base)

  scale (scaled_index -> Just index) (sib_base_with_disp -> Just base) (Disp32 d)
    -> ret_sib 0b10 (Disp32 d) (s scale index base)

  _ _ _ _ -> Nothing

  where
    ret a b c d = Just (mkModRM_mod_rm a b,c,d)
    ret_sib a c d = ret a 0b100 c d
    s a b c   = Just (mkSIB a b c)
    sib_base_without_disp = sib_base False
    sib_base_with_disp    = sib_base True
    sib_base with_disp = \case
      JR_EAX             -> Just 0b000
      JR_ECX             -> Just 0b001
      JR_EDX             -> Just 0b010
      JR_EBX             -> Just 0b011
      JR_ESP             -> Just 0b100
      JR_EBP | with_disp -> Just 0b101 -- EBP requires a disp
      JR_ESI             -> Just 0b110
      JR_EDI             -> Just 0b111
      _                  -> Nothing
    scaled_index = \case
      JR_EAX  -> Just 0b000
      JR_ECX  -> Just 0b001
      JR_EDX  -> Just 0b010
      JR_EBX  -> Just 0b011
      Nothing -> Just 0b100
      JR_EBP  -> Just 0b101
      JR_ESI  -> Just 0b110
      JR_EDI  -> Just 0b111
      _       -> Nothing

-- | Return (modrm,disp,sib,rex) for the given 64-bit addressing form scale-index-base-disp
encodeMem64 :: Scale -> Maybe Reg -> Maybe Reg -> Disp -> Maybe (ModRM,Disp,Maybe SIB,Maybe Rex)
encodeMem64 scale index base disp
  -- 2-scale without base and disp: transform 2*r ==> r+r to avoid requiring a 0 disp
  -- only if r /= RSP/RBP
  | Scale2 <- scale
  , Nothing <- base
  , NoDisp <- disp
  , index /= Nothing && index /= JR_RSP && index /= JR_RBP
  = encodeMem64 Scale1 index index NoDisp

  -- base case
  | Just r <- encode_mem64 scale index base disp
  = Just r

  -- 1-scale without base: try using the index as base (if not RSP/RBP)
  | Scale1 <- scale
  , Nothing <- base
  , index /= Nothing && index /= JR_RSP && index /= JR_RBP
  = encodeMem64 Scale1 Nothing index disp

  -- try to sign-extend disp16
  | Disp16 d <- disp
  = encodeMem64 scale index base (Disp32 (i32FromI16 d))

  -- try to sign-extend disp8
  | Disp8 d <- disp
  = encodeMem64 scale index base (Disp32 (i32FromI8 d))

  -- try with a 0 disp
  | NoDisp <- disp
  = encodeMem64 scale index base (Disp8 0)

  | otherwise
  = Nothing

-- | Return (modrm,disp,sib,rex) for the given 64-bit addressing form scale-index-base-disp
--
-- modrm.reg is set to 0
--
-- This function doesn't try to arrange its arguments differently to make them
-- encodable. Use encodeMem64 for this
encode_mem64 :: Scale -> Maybe Reg -> Maybe Reg -> Disp -> Maybe (ModRM,Disp,Maybe SIB,Maybe Rex)
encode_mem64 = \cases
  -- without SIB byte
  _      Nothing JR_RAX  NoDisp      -> ret 0b00 0b000 NoDisp     Nothing Nothing
  _      Nothing JR_RCX  NoDisp      -> ret 0b00 0b001 NoDisp     Nothing Nothing
  _      Nothing JR_RDX  NoDisp      -> ret 0b00 0b010 NoDisp     Nothing Nothing
  _      Nothing JR_RBX  NoDisp      -> ret 0b00 0b011 NoDisp     Nothing Nothing
  _      Nothing Nothing (Disp32 d)  -> ret 0b00 0b101 (Disp32 d) Nothing Nothing
  _      Nothing JR_RSI  NoDisp      -> ret 0b00 0b110 NoDisp     Nothing Nothing
  _      Nothing JR_RDI  NoDisp      -> ret 0b00 0b111 NoDisp     Nothing Nothing
  _      Nothing JR_R8   NoDisp      -> ret 0b00 0b000 NoDisp     Nothing (Just rexB)
  _      Nothing JR_R9   NoDisp      -> ret 0b00 0b001 NoDisp     Nothing (Just rexB)
  _      Nothing JR_R10  NoDisp      -> ret 0b00 0b010 NoDisp     Nothing (Just rexB)
  _      Nothing JR_R11  NoDisp      -> ret 0b00 0b011 NoDisp     Nothing (Just rexB)
          -- no R12: it requires a SIB
          -- no R13: it requires a disp
  _      Nothing JR_R14  NoDisp      -> ret 0b00 0b110 NoDisp     Nothing (Just rexB)
  _      Nothing JR_R15  NoDisp      -> ret 0b00 0b111 NoDisp     Nothing (Just rexB)

  _      Nothing JR_RAX  (Disp8 d)   -> ret 0b01 0b000 (Disp8 d)  Nothing Nothing
  _      Nothing JR_RCX  (Disp8 d)   -> ret 0b01 0b001 (Disp8 d)  Nothing Nothing
  _      Nothing JR_RDX  (Disp8 d)   -> ret 0b01 0b010 (Disp8 d)  Nothing Nothing
  _      Nothing JR_RBX  (Disp8 d)   -> ret 0b01 0b011 (Disp8 d)  Nothing Nothing
  _      Nothing JR_RBP  (Disp8 d)   -> ret 0b01 0b101 (Disp8 d)  Nothing Nothing
  _      Nothing JR_RSI  (Disp8 d)   -> ret 0b01 0b110 (Disp8 d)  Nothing Nothing
  _      Nothing JR_RDI  (Disp8 d)   -> ret 0b01 0b111 (Disp8 d)  Nothing Nothing
  _      Nothing JR_R8   (Disp8 d)   -> ret 0b01 0b000 (Disp8 d)  Nothing (Just rexB)
  _      Nothing JR_R9   (Disp8 d)   -> ret 0b01 0b001 (Disp8 d)  Nothing (Just rexB)
  _      Nothing JR_R10  (Disp8 d)   -> ret 0b01 0b010 (Disp8 d)  Nothing (Just rexB)
  _      Nothing JR_R11  (Disp8 d)   -> ret 0b01 0b011 (Disp8 d)  Nothing (Just rexB)
          -- no R12: it requires a SIB
  _      Nothing JR_R13  (Disp8 d)   -> ret 0b01 0b101 (Disp8 d)  Nothing (Just rexB)
  _      Nothing JR_R14  (Disp8 d)   -> ret 0b01 0b110 (Disp8 d)  Nothing (Just rexB)
  _      Nothing JR_R15  (Disp8 d)   -> ret 0b01 0b111 (Disp8 d)  Nothing (Just rexB)

  _      Nothing JR_RAX  (Disp32 d)  -> ret 0b10 0b000 (Disp32 d) Nothing Nothing
  _      Nothing JR_RCX  (Disp32 d)  -> ret 0b10 0b001 (Disp32 d) Nothing Nothing
  _      Nothing JR_RDX  (Disp32 d)  -> ret 0b10 0b010 (Disp32 d) Nothing Nothing
  _      Nothing JR_RBX  (Disp32 d)  -> ret 0b10 0b011 (Disp32 d) Nothing Nothing
  _      Nothing JR_RBP  (Disp32 d)  -> ret 0b10 0b101 (Disp32 d) Nothing Nothing
  _      Nothing JR_RSI  (Disp32 d)  -> ret 0b10 0b110 (Disp32 d) Nothing Nothing
  _      Nothing JR_RDI  (Disp32 d)  -> ret 0b10 0b111 (Disp32 d) Nothing Nothing
  _      Nothing JR_R8   (Disp32 d)  -> ret 0b10 0b000 (Disp32 d) Nothing (Just rexB)
  _      Nothing JR_R9   (Disp32 d)  -> ret 0b10 0b001 (Disp32 d) Nothing (Just rexB)
  _      Nothing JR_R10  (Disp32 d)  -> ret 0b10 0b010 (Disp32 d) Nothing (Just rexB)
  _      Nothing JR_R11  (Disp32 d)  -> ret 0b10 0b011 (Disp32 d) Nothing (Just rexB)
          -- no R12: it requires a SIB
  _      Nothing JR_R13  (Disp32 d)  -> ret 0b10 0b101 (Disp32 d) Nothing (Just rexB)
  _      Nothing JR_R14  (Disp32 d)  -> ret 0b10 0b110 (Disp32 d) Nothing (Just rexB)
  _      Nothing JR_R15  (Disp32 d)  -> ret 0b10 0b111 (Disp32 d) Nothing (Just rexB)

  -- with SIB byte
  scale (scaled_index -> Just (rexi,index)) Nothing (Disp32 d)
    -> ret_sib 0b00 (Disp32 d) (s scale index 0b101) rexi

  scale (scaled_index -> Just (rexi,index)) (sib_base_without_disp -> Just (rexb,base)) NoDisp
    -> ret_sib 0b00 NoDisp (s scale index base) (rexi <> rexb)

  scale (scaled_index -> Just (rexi,index)) (sib_base_with_disp -> Just (rexb,base)) (Disp8 d)
    -> ret_sib 0b01 (Disp8 d) (s scale index base) (rexb <> rexi)

  scale (scaled_index -> Just (rexi,index)) (sib_base_with_disp -> Just (rexb,base)) (Disp32 d)
    -> ret_sib 0b10 (Disp32 d) (s scale index base) (rexb <> rexi)

  _ _ _ _ -> Nothing

  where
    ret a b c d e = Just (mkModRM_mod_rm a b,c,d,e)
    ret_sib a c d e = ret a 0b100 c d e
    s a b c   = Just (mkSIB a b c)
    sib_base_without_disp = sib_base False
    sib_base_with_disp    = sib_base True
    sib_base with_disp = \case
      JR_RAX             -> Just (Nothing  ,0b000)
      JR_RCX             -> Just (Nothing  ,0b001)
      JR_RDX             -> Just (Nothing  ,0b010)
      JR_RBX             -> Just (Nothing  ,0b011)
      JR_RSP             -> Just (Nothing  ,0b100)
      JR_RBP | with_disp -> Just (Nothing  ,0b101) -- RBP requires a disp
      JR_RSI             -> Just (Nothing  ,0b110)
      JR_RDI             -> Just (Nothing  ,0b111)
      JR_R8              -> Just (Just rexB,0b000)
      JR_R9              -> Just (Just rexB,0b001)
      JR_R10             -> Just (Just rexB,0b010)
      JR_R11             -> Just (Just rexB,0b011)
      JR_R12             -> Just (Just rexB,0b100)
      JR_R13             -> Just (Just rexB,0b101)
      JR_R14             -> Just (Just rexB,0b110)
      JR_R15             -> Just (Just rexB,0b111)
      _                  -> Nothing
    scaled_index = \case
      JR_RAX  -> Just (Nothing  ,0b000)
      JR_RCX  -> Just (Nothing  ,0b001)
      JR_RDX  -> Just (Nothing  ,0b010)
      JR_RBX  -> Just (Nothing  ,0b011)
      Nothing -> Just (Nothing  ,0b100)
      JR_RBP  -> Just (Nothing  ,0b101)
      JR_RSI  -> Just (Nothing  ,0b110)
      JR_RDI  -> Just (Nothing  ,0b111)
      JR_R8   -> Just (Just rexX,0b000)
      JR_R9   -> Just (Just rexX,0b001)
      JR_R10  -> Just (Just rexX,0b010)
      JR_R11  -> Just (Just rexX,0b011)
      JR_R12  -> Just (Just rexX,0b100)
      JR_R13  -> Just (Just rexX,0b101)
      JR_R14  -> Just (Just rexX,0b110)
      JR_R15  -> Just (Just rexX,0b111)
      _       -> Nothing


-- | Return (modrm,disp) for the given 64-bit RIP-relative addressing form RIP+disp
encodeMemRel :: Disp -> Maybe (ModRM,Disp)
encodeMemRel disp =
  let !disp' = case disp of
                NoDisp   -> 0
                Disp32 d -> d
                Disp16 d -> (i32FromI16 d)
                Disp8  d -> (i32FromI8 d)
  in Just (mkModRM_mod_rm 0b00 0b101, Disp32 disp')

-- | Memory addressing
data Mem = Mem
  { memSegment  :: !(Maybe Segment)        -- ^ Segment override
  , memSize     :: !(Maybe Size)           -- ^ Target memory size
  , memLock     :: !MemLock                -- ^ Memory lock
  , memAddrSize :: !(Maybe AddressSize)    -- ^ Overloaded address size
  , memAddr     :: !MemAddr                -- ^ Memory address
  }
  deriving (Eq,Ord)

instance Show Mem where
  show (Mem s sz l asz a) = concat
    [ case asz of
        Nothing   -> ""
        Just asz' -> show asz' ++ "@"
    , "["
    , intercalate ":" $ catMaybes
      [ fmap show s
      , fmap (show . sizeInBits) sz
      , case l of
          Lock -> Just "lock"
          NoLock -> Nothing
      , Just (show a)
      ]
    , "]"
    ]

emptyMem :: Mem
emptyMem = Mem
  { memSegment = Nothing
  , memSize = Nothing
  , memLock = NoLock
  , memAddrSize = Nothing
  , memAddr = MemAbs
    { mabsBase = Nothing
    , mabsScale = Scale1
    , mabsIndex = Nothing
    , mabsDisp = NoDisp
    }
  }

data MemAddr
  -- | Absolute memory address (i.e. non relative to rIP)
  = MemAbs
    { mabsBase  :: !(Maybe Reg) -- ^ Base register
    , mabsScale :: !Scale       -- ^ Scale for the index
    , mabsIndex :: !(Maybe Reg) -- ^ Index register
    , mabsDisp  :: !Disp        -- ^ Displacement
    }
  -- | rIP relative address
  | MemRel
    { mrelDisp :: Disp -- ^ Displacement relative to rIP
    }
  deriving (Eq,Ord)

instance Show MemAddr where
  show = \case
    MemRel d -> intercalate " + " $ catMaybes [Just "RIP", showDispMaybe d]
    MemAbs b s i d -> intercalate "+" $ catMaybes
                        [ fmap show b
                        , fmap (\x -> showScale s ++ "*" ++ show x) i
                        , showDispMaybe d
                        ]



isMemAny :: Mem -> Maybe Mem
isMemAny m = if memSize m == Nothing then Just m else Nothing

isMem8 :: Mem -> Maybe Mem
isMem8 m = if memSize m == Just Size8 then Just m else Nothing

isMem16 :: Mem -> Maybe Mem
isMem16 m = if memSize m == Just Size16 then Just m else Nothing

isMem32 :: Mem -> Maybe Mem
isMem32 m = if memSize m == Just Size32 then Just m else Nothing

isMem64 :: Mem -> Maybe Mem
isMem64 m = if memSize m == Just Size64 then Just m else Nothing

isMem128 :: Mem -> Maybe Mem
isMem128 m = if memSize m == Just Size128 then Just m else Nothing

isMem256 :: Mem -> Maybe Mem
isMem256 m = if memSize m == Just Size256 then Just m else Nothing

-- | Encode a memory operand. Return Nothing in case of failure to encode.
encodeMem :: Context -> Mem -> Enc -> Maybe Enc
encodeMem ctx mem e = do
  addr_prefix <- case memAddrSize mem of
      Nothing -> Just id
      Just a
        | overriddenAddressSize False ctx == a -> Just id
        | overriddenAddressSize True  ctx == a -> Just (P_67 :)
        | otherwise                            -> Nothing -- can't override address-size
  let lock_prefix = case memLock mem of
        Lock   -> (P_F0 :)
        NoLock -> id
  let seg_prefix = case memSegment mem of
        Nothing -> id
        Just s  -> (segmentOverridePrefix s :)
  let add_prefixes = addr_prefix . lock_prefix . seg_prefix . encPrefixes

  case effectiveMode (ctxMode ctx) of
    EMode64
      | MemRel d <- memAddr mem
      , Just (modrm,disp) <- encodeMemRel d
      -> Just $ e { encModRM    = encModRM e <> Just modrm
                  , encDisp     = disp
                  , encPrefixes = add_prefixes e
                  }

      | MemAbs b sc i d <- memAddr mem
      , Just (modrm,disp,sib,rex) <- encodeMem64 sc i b d
      -> Just $ e { encModRM    = encModRM e <> Just modrm
                  , encDisp     = disp
                  , encSIB      = sib
                  , encRex      = encRex e <> rex
                  , encPrefixes = add_prefixes e
                  }

    EMode32
      | MemAbs b sc i d <- memAddr mem
      , Just (modrm,disp,sib) <- encodeMem32 sc i b d
      -> Just $ e { encModRM    = encModRM e <> Just modrm
                  , encDisp     = disp
                  , encSIB      = sib
                  , encPrefixes = add_prefixes e
                  }

    EMode16
      | MemAbs b Scale1 i d <- memAddr mem
      , Just (modrm,disp) <- encodeMem16 i b d
      -> Just $ e { encModRM    = encModRM e <> Just modrm
                  , encDisp     = disp
                  , encPrefixes = add_prefixes e
                  }

    _ -> Nothing


data MemLock
  = Lock
  | NoLock
  deriving (Show,Eq,Ord)

