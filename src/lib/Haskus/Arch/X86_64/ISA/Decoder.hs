{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiWayIf #-}

module Haskus.Arch.X86_64.ISA.Decoder
   ( getInstruction
   )
where

import Haskus.Arch.X86_64.ISA.Mode
import Haskus.Arch.X86_64.ISA.Registers
import Haskus.Arch.X86_64.ISA.RegisterNames
import Haskus.Arch.Common.Register hiding (Reg)
import Haskus.Arch.X86_64.ISA.Size
import Haskus.Arch.X86_64.ISA.Solver
import Haskus.Arch.X86_64.ISA.OpcodeMaps
import Haskus.Arch.X86_64.ISA.Insns
import Haskus.Arch.X86_64.ISA.Insn
import Haskus.Arch.X86_64.ISA.Encoding

import Haskus.Format.Binary.Bits
import Haskus.Format.Binary.Word
import Haskus.Format.Binary.Get
import Haskus.Format.Binary.BitField
import qualified Haskus.Format.Binary.BitSet as BitSet

import Haskus.Utils.Solver
import Haskus.Utils.List (nub, (\\))
import Haskus.Utils.Maybe
import Haskus.Utils.Flow

import qualified Data.Map.Strict as Map
import qualified Data.Vector as V

-- ===========================================================================
-- X86 Instruction
-- ===========================================================================


getInstruction :: ExecMode -> Get Insn
getInstruction mode = consumeAtMost 15 $ do
   -- An instruction is at most 15 bytes long

   ps  <- readLegacyPrefixes
   rex <- readRexPrefix mode

   -- read opcode
   oc <- readVexXopOpcode mode ps rex >>= \case
      Just op -> return op
      Nothing -> readLegacyOpcode mode ps rex

   case opcodeMap oc of

      -- Handle 3DNow! encoding: the opcode byte is the last byte of the
      -- instruction and the operand encoding is predefined (not opcode
      -- specific)
      MapLegacy Map3DNow -> do
         ops <- readOperands mode ps oc amd3DNowEncoding
         -- read opcode byte
         let OpLegacy ps' rx ocm _ = oc
         oc' <- OpLegacy ps' rx ocm <$> getWord8
         return $ Insn oc'
                ops
                amd3DNowEncoding
                (error "3DNow! instructions not supported") -- TODO
                BitSet.empty

      ocmap              -> do
         -- get candidate instructions for the opcode
         cs <- case Map.lookup ocmap opcodeMaps of
                  Nothing -> fail "No opcode map found"
                  Just t  -> return (t V.! fromIntegral (opcodeByte oc))

         when (null cs) $ fail "No candidate instruction found (empty opcode map cell)"

         -- check prefixes
         let
            isPrefixValid e x = Just x == encMandatoryPrefix e
               || case x of
                  -- operand-size prefix
                  LegacyPrefix66 -> encHasVariableSizedOperand e
                  -- address-size prefix
                  LegacyPrefix67 -> encMayHaveMemoryOperand e
                  -- CS segment override / Branch not taken hint
                  LegacyPrefix2E -> encMayHaveMemoryOperand e
                                    || encBranchHintable e
                  -- DS segment override / Branch taken hint
                  LegacyPrefix3E -> encMayHaveMemoryOperand e
                                    || encBranchHintable e
                  -- ES segment override
                  LegacyPrefix26 -> encMayHaveMemoryOperand e 
                  -- FS segment override
                  LegacyPrefix64 -> encMayHaveMemoryOperand e 
                  -- GS segment override
                  LegacyPrefix65 -> encMayHaveMemoryOperand e 
                  -- SS segment override
                  LegacyPrefix36 -> encMayHaveMemoryOperand e 
                  -- LOCK prefix
                  LegacyPrefixF0 -> encLockable e
                  -- REPZ / XRELEASE
                  LegacyPrefixF3 -> encRepeatable e
                                    || encSupportHLE XRelease e
                  -- REPNZ / XACQUIRE
                  LegacyPrefixF2 -> encRepeatable e
                                    || encSupportHLE XAcquire e

            arePrefixesValid c = all (isPrefixValid (entryEncoding c)) ps

            cs2 = filter (\c -> hasMandatoryPrefix c && arePrefixesValid c) cs
            hasMandatoryPrefix i = case (encMandatoryPrefix (entryEncoding i), oc) of
               (mp, OpVex v _)        -> mp == vexPrefix v
               (mp, OpXop v _)        -> mp == vexPrefix v
               (Just mp, OpLegacy {}) -> mp `elem` ps
               (Nothing, _          ) -> True

         when (null cs2) $ fail "No candidate instruction found (invalid mandatory prefixes)"

         -- try to read ModRM
         modrm <- lookAhead $ remaining >>= \case
            x | x >= 1 -> Just <$> getWord8
            _          -> return Nothing

         -- filter out invalid opcode extensions:
         let
            cs3 = filter hasOpcodeExtension cs2
            hasOpcodeExtension i = fullext && regext && wext && lext
               where
                  e = entryEncoding i
                  -- extension in the whole second byte
                  fullext = case (modrm,encOpcodeFullExt e) of
                     (_, Nothing)      -> True
                     (Just m, Just x)  -> m == x
                     (Nothing, Just _) -> False
                  -- extension in ModRM.Reg
                  regext = case (modrm,encOpcodeExt e) of
                     (_, Nothing)      -> True
                     (Just m, Just x)  -> regField (ModRM (BitFields m)) == x
                     (Nothing, Just _) -> False
                  -- extension in REX.W, VEX.W, etc.
                  wext = case encOpcodeWExt e of
                     Nothing -> True
                     Just x  -> opcodeW oc == x
                  -- extension in VEX.L, etc.
                  lext = case encOpcodeLExt e of
                     Nothing -> True
                     Just x  -> opcodeL oc == Just x

         when (null cs3) $ fail ("No candidate instruction found (opcode extension filtering): " ++ show oc)

         -- filter out invalid ModRM.mod (e.g., only 11b)
         let
            cs4 = filter hasValidMod cs3
            hasValidMod i = case (modrm, encValidModRMMode (entryEncoding i)) of
               (Nothing, vm) -> vm == ModeNone
               (Just m,  vm) -> case vm of
                     ModeOnlyReg -> m' == 0b11
                     ModeOnlyMem -> m' /= 0b11
                     _           -> True
                  where m' = m `shiftR` 6

         when (null cs4) $ fail "No candidate instruction found (ModRM.mod filtering)"

         -- Filter out invalid enabled extensions
         -- and invalid execution mode
         let
            cs5 = filter (encSupportExecMode mode . entryEncoding) cs4
               
         when (null cs5) $ do
            -- get disabled extensions that may have filtered out the expected
            -- encoding
            let es = nub (concatMap (encRequiredExtensions . entryEncoding) cs4) \\ extensions mode

            fail ("No candidate instruction found, try enabling one of: "++ show es)

         -- If there are more than one instruction left, signal a bug
         MapEntry spec enc <- case cs5 of
            [x] -> return x
            xs  -> fail ("More than one instruction found (opcode table bug?): " ++ show (fmap (insnMnemonic . entryInsn) xs))

         -- Read params
         ops <- readOperands mode ps oc enc

         -- Variants
         let
            -- lock prefix
            vlocked  = if encLockable enc && LegacyPrefixF0 `elem` ps
                        then BitSet.singleton Locked
                        else BitSet.empty

            -- repeat prefixes
            vrepeat  = if encRepeatable enc
                        then if LegacyPrefixF3 `elem` ps
                           then BitSet.singleton RepeatZero
                           else if LegacyPrefixF2 `elem` ps
                              then BitSet.singleton RepeatNonZero
                              else BitSet.empty
                        else BitSet.empty

            -- branch hint prefixes
            vbranchhint = if encBranchHintable enc
                        then if LegacyPrefix3E `elem` ps
                           then BitSet.singleton BranchHintTaken
                           else if LegacyPrefix2E `elem` ps
                              then BitSet.singleton BranchHintNotTaken
                              else BitSet.empty
                        else BitSet.empty

            -- check if insn is reversable, if the reversable bit is set
            -- and if there are only registers operands (because it is the only
            -- case for which there are two different encodings for the same
            -- instruction:
            --    ModRM.reg = r1, ModRM.rm = r2, reversed = False
            --    ModRM.reg = r2, ModRM.rm = r1, reversed = True
            isRegOp (OpReg _) = True
            isRegOp _         = False
            onlyRegOps        = all isRegOp ops
            reversed = case encReversableBit enc of
               Just b  -> testBit (opcodeByte oc) b 
               Nothing -> False

            vreverse = if reversed && onlyRegOps
               then BitSet.singleton Reversed
               else BitSet.empty

            -- TODO: superfluous segment override
            -- TODO: explicit param variant

            variants = BitSet.unions [ vlocked
                                     , vreverse 
                                     , vrepeat
                                     , vbranchhint
                                     ]

         return $ Insn oc ops enc spec variants

-- ===========================================================================
-- Legacy encoding
-- ===========================================================================

---------------------------------------------------------------------------
-- Legacy prefixes
-- ~~~~~~~~~~~~~~~
-- 
-- An instruction optionally begins with up to five legacy prefixes, in any
-- order. These prefixes can:
--    1) modify the instruction's default address size
--    2) modify the instruction's default operand size
--    3) modify the instruction's memory address segment
--    4) be used as an opcode extension
--    5) provide atomic bus locking or hardware-lock elision (HLE)
--    6) repeat the instruction until a condition is met
--
-- Note: the effective sizes of the operands may not be the same: the shorter
-- may be sign-extended or zero-extended.
--
-- Legacy prefixes that are used as opcode extensions are mandatory.
--
--
-- Legacy prefix groups
-- ~~~~~~~~~~~~~~~~~~~~
--
-- Legacy prefixes are organized in five groups. An instruction may include
-- at most one prefix from each group. The result of using multiple prefixes
-- from a single group is undefined.
--
-- We give the original meaning in parentheses, but prefixes can be used with
-- other meanings.
--
-- G1: 0x66 (Operand-size override)
-- G2: 0x67 (Address-size override)
-- G3: 0x2E (CS segment override)
--     0x3E (DS segment override)
--     0x26 (ES segment override)
--     0x64 (FS segment override)
--     0x65 (GS segment override)
--     0x36 (SS segment override)
-- G4: 0xF0 (atomic memory access (lock))
-- G5: 0xF3 (repeat while zero)
--     0xF2 (repeat while non-zero)
--
-- New opcode encodings (VEX, XOP, etc.) only support legacy prefixes from
-- groups G2 and G3.
---------------------------------------------------------------------------

-- | Read legacy prefixes (up to 5)
readLegacyPrefixes :: Get [LegacyPrefix]
readLegacyPrefixes = do
   let
      readLegacyPrefix :: Get (Maybe LegacyPrefix)
      readLegacyPrefix = lookAheadM (toLegacyPrefix <$> getWord8)

      -- | Check that legacy prefixes belong to different groups
      checkLegacyPrefixes :: [LegacyPrefix] -> Bool
      checkLegacyPrefixes ps =
         length ps == length (nub (map legacyPrefixGroup ps))

   -- read at most 5 legacy prefixes
   ws <- getManyAtMost 5 readLegacyPrefix

   -- check that legacy prefixes are valid (group-wise)
   if checkLegacyPrefixes ws
      then return ws
      else fail ("Invalid legacy prefixes: " ++ show ws)
   
-- | Get the legacy prefix group
legacyPrefixGroup :: LegacyPrefix -> Int
legacyPrefixGroup = \case
   LegacyPrefix66  -> 1
   LegacyPrefix67  -> 2
   LegacyPrefix2E  -> 3
   LegacyPrefix3E  -> 3
   LegacyPrefix26  -> 3
   LegacyPrefix64  -> 3
   LegacyPrefix65  -> 3
   LegacyPrefix36  -> 3
   LegacyPrefixF0  -> 4
   LegacyPrefixF3  -> 5
   LegacyPrefixF2  -> 5


---------------------------------------------------------------------------
-- REX prefix
-- ~~~~~~~~~~
--
-- In 64-bit mode, a REX prefix can be used after the legacy prefixes. A REX
-- prefix contains several fields, hence it ranges from 0x40 to 0x4F. In
-- non-64-bit mode, this range is used by the short variants of the INC/DEC
-- instructions, hence these forms are not usable in 64-bit mode.
--
-- There are several things to consider:
--    1) whether a REX prefix is present (whatever it contains)
--    2) the contents of the REX prefix fields.
--
-- Its presence implies:
--    - the use of the uniform byte registers (SIL, DIL, etc. instead of AH,
--    BH, etc.)
--
-- Its fields indicate:
--    - the use of the extended registers (one additional bit per register)
--    - the use of a 64-bit operand size (ignoring the operand-size overriding
--    legacy prefix)
--
-- Some instructions have default or fixed operand size set to 64bits in 64-bit
-- mode, hence they don't require the REX prefix.
--
-- The prefix has the following format:
--
-- |    4    | W | R | X | B |
--                         ^-- base register or ModRM.rm extension
--                     ^------ SIB.index register extension
--                 ^---------- ModRM.reg register extension
--             ^-------------- set to 1 for 64-bit operand size
--      ^--------------------- 4 bits set to 0xD
--
-- 
-- If more than one REX prefix is present, the behavior is undefined (however it
-- seems that the last one is used).
-- 
---------------------------------------------------------------------------

-- | Read optional REX prefix
readRexPrefix :: ExecMode -> Get (Maybe Rex)
readRexPrefix mode =
   
   -- REX is only supported in 64-bit mode
   if is64bitMode (x86Mode mode)
      then lookAheadM $ do
         x <- getWord8
         return $ if isRexPrefix x
            then Just (Rex x)
            else Nothing
      else return Nothing

---------------------------------------------------------------------------
-- Legacy opcodes
-- ~~~~~~~~~~~~~~
--
-- Legacy opcode can belong to one of the following opcode maps:
--    - Primary
--    - 0x0F
--    - 0x0F38
--    - 0x0F3A
--    - 3DNow! (escaped with 0x0F0F, opcode byte in last instruction byte)
--
---------------------------------------------------------------------------

-- | Read legacy opcode
readLegacyOpcode :: ExecMode -> [LegacyPrefix] -> Maybe Rex -> Get Opcode
readLegacyOpcode mode ps rex = do

   let
      is3DNowAllowed = mode `hasExtension` AMD3DNow
      ret m x = return (OpLegacy ps rex m x)

   getWord8 >>= \case
      0x0F -> getWord8 >>= \case
         -- the real 3DNow! opcode is stored in the last byte and
         -- will be set later
         0x0F | is3DNowAllowed -> ret Map3DNow 0
         0x3A                  -> ret Map0F3A =<< getWord8
         0x38                  -> ret Map0F38 =<< getWord8
         w2                    -> ret Map0F w2
      w1   -> ret MapPrimary w1

-- ===========================================================================
-- VEX/XOP encodings
-- ===========================================================================

---------------------------------------------------------------------------
-- VEX/XOP prefixes
-- ~~~~~~~~~~~~~~~~
--
-- VEX/XOP prefixes are different from the REX prefix: they don't extend
-- existing instructions but add new ones (new opcode maps). Moreover they are
-- mutually exclusive with the REX prefix as they subsume it.
--
-- Some legacy prefixes are supported: address-size and segment override.
--
---------------------------------------------------------------------------

-- | Read VEX/XOP encoded opcode
readVexXopOpcode :: ExecMode -> [LegacyPrefix] -> Maybe Rex -> Get (Maybe Opcode)
readVexXopOpcode mode ps rex = do
   let
      isXOPAllowed = mode `hasExtension` XOP
      isVEXAllowed = mode `hasExtension` VEX

      -- VEX prefixes are supported in 32-bit and 16-bit modes
      -- They overload LES and LDS opcodes so that the first two bits
      -- of what would be ModRM are invalid (11b) for LES/LDS
      testMod :: Word8 -> Bool
      testMod w    = w `unsafeShiftR` 6 == 0x03

      isVexMode act = do
         c <- if is64bitMode (x86Mode mode)
                  then return True
                  else testMod <$> lookAhead getWord8
         if c
            then act
            else return Nothing

      -- Legacy prefixes in groups other than 2 or 3 aren't supported with
      -- VEX/XOP encoding. REX prefix isn't supported either.
      -- This function checks this
      checkVexPrefixes act = do
         let ps' = filter (\x -> legacyPrefixGroup x /= 2 
                              && legacyPrefixGroup x /= 3) ps
         case ps' of
            [] -> case rex of
               Nothing -> Just <$> act
               _       -> fail "REX prefix found with VEX/XOP opcode"
            _  -> fail ("Invalid legacy prefixes found with VEX/XOP opcode: "
                           ++ show ps')

   lookAheadM $ getWord8 >>= \case
      0x8F  |  isXOPAllowed -> checkVexPrefixes $
                  OpXop <$> (Vex3 <$> getWord8 <*> getWord8) <*> getWord8

      0xC4  |  isVEXAllowed -> isVexMode $ checkVexPrefixes $
                  OpVex <$> (Vex3 <$> getWord8 <*> getWord8) <*> getWord8

      0xC5  |  isVEXAllowed -> isVexMode $ checkVexPrefixes $
                  OpVex <$> (Vex2 <$> getWord8) <*> getWord8

      _ -> return Nothing

      
-- ===========================================================================
-- Operands
-- ===========================================================================

-- | Read instruction operands
readOperands :: ExecMode -> [LegacyPrefix] -> Opcode -> Encoding -> Get [Operand]
readOperands mode ps oc enc = do

   -- read ModRM
   modrm <- if encRequireModRM enc
            then (Just . ModRM . BitFields) <$> getWord8
            else return Nothing


   let
      -- we compute the overriden address size. It depends on:
      --    * the default address size
      --    * the presence of the 0x67 legacy prefix
      hasPrefix67 = LegacyPrefix67 `elem` ps
      addressSize = overriddenAddressSize hasPrefix67 mode

      -- we determine the effective operand size. It depends on:
      --   * the mode of execution
      --   * the presence of the 0x66 legacy prefix
      --   * the default operand size of the instruction in 64-bit mode (64-bit
      --   or not)
      --   * the value of the ForceNo8bit bit in the opcode (if applicable)
      --   * the value of REX.W/VEX.W/XOP.W (if applicable)
      hasPrefix66     = LegacyPrefix66 `elem` ps
      hasDefaultOp64  = DefaultOperandSize64 `elem` encProperties enc
      hasRexW         = opcodeW oc

      oos64 = overriddenOperationSize64 hasPrefix66 hasRexW hasDefaultOp64 mode

      --finally we take into account the NoForce8bit bit in the opcode
      hasForce8bit = case encNoForce8Bit enc of
         Just b | not (testBit (opcodeByte oc) b) -> True
         _                                        -> False

      operandSize = if hasForce8bit then OpSize8 else oos64

      -- do we need to read an SIB byte?
      hasSIB = fromMaybe False (useSIB addressSize <$> modrm)

      -- do we need to read a MemOffset displacement?
      hasMemOffset = T_MemOffset `elem` fmap opType (encOperands enc)

      -- do we need to read a Relative displacement?
      hasRelOffset = filter isRel (fmap opType (encOperands enc))
         where isRel (T_Rel _) = True
               isRel _         = False

   -- read SIB byte if necessary
   sib <- if hasSIB
      then (Just . SIB) <$> getWord8
      else return Nothing

   let
      -- do we need to read a displacement? Which size?
      dispSize = case (hasMemOffset, hasRelOffset) of
         (False, []) -> join (useDisplacement addressSize sib <$> modrm)
         (True,  []) -> case addressSize of
                           AddrSize16 -> Just Size16
                           AddrSize32 -> Just Size32
                           AddrSize64 -> Just Size64
         (False, [T_Rel rel]) -> case rel of
            Rel8     -> Just Size8
            Rel16o32 -> case operandSize of
               OpSize64 -> Just Size32
               OpSize32 -> Just Size32
               OpSize16 -> Just Size16
               OpSize8  -> error "Unsupported relative offset with 8-bit operand size"
         (_, xs) -> error ("Unsupported relative offsets: " ++ show xs)


   -- read displacement if necessary
   disp <- forM dispSize getSize


   let
      -- do we need to read some immediates?
      immTypeSize = \case
         T_MemOffset     -> [] -- already read above as displacement
         T_Rel _         -> [] -- already read above as displacement
         T_Imm ImmSize8  -> [Size8]
         T_Imm ImmSize16 -> [Size16]
         T_Imm ImmSizeOp -> case operandSize of
                              OpSize8  -> [Size8]
                              OpSize16 -> [Size16]
                              OpSize32 -> [Size32]
                              OpSize64 -> [Size64]
         T_Imm ImmSizeSE -> case encSignExtendImmBit enc of
                              -- if the sign-extendable bit is set, we read an
                              -- Imm8 that will be sign-extended to match the
                              -- operand size
                              Just t
                                 | testBit (opcodeByte oc) t -> [Size8]
                              _ -> case operandSize of
                                 OpSize8  -> [Size8]
                                 OpSize16 -> [Size16]
                                 OpSize32 -> [Size32]
                                 OpSize64 -> [Size32] -- sign-extended
         T_Pair x y      -> [ head (immTypeSize x) -- immediate pointer: 16:16 or 16:32
                            , head (immTypeSize y)]
         it              -> error ("Unhandled immediate type: " ++ show it)

      immSize x = case opEnc x of
         Imm8h -> [Size8]
         Imm8l -> [Size8]
         Imm   -> immTypeSize (opType x)
         _     -> error ("unhandled immediate encoding: " ++ show x)

      immSizes = case filter (isImmediate . opEnc) (encOperands enc) of
         []    -> []
         [x]   -> immSize x
         [x,y] 
            | opEnc x == Imm8h && opEnc y == Imm8l -> [Size8]
            | opEnc x == Imm8l && opEnc y == Imm8h -> [Size8]
         xs    -> concatMap immSize xs

   -- read immediates if necessary
   imms <- forM immSizes getSize

   ----------------------------------------------------------------------------
   -- at this point we have read the whole instruction (except in the 3DNow!
   -- case where there is a single byte left to read). Now we can determine the
   -- operands.
   ----------------------------------------------------------------------------

   let
      is64bitMode' = is64bitMode (x86Mode mode)

      -- predicate oracle
      oracle = makeOracle <|
         (fmap (\m -> (ContextPred (Mode m), UnsetPred))
               (filter (/= x86Mode mode) allModes))
         ++
         [ (ContextPred (Mode (x86Mode mode)), SetPred)
         , (ContextPred CS_D         , if | is64bitMode'           -> UnsetPred
                                          | csDescriptorFlagD mode -> SetPred
                                          | otherwise              -> UnsetPred)
         , (ContextPred SS_B         , if | ssDescriptorFlagB mode -> SetPred
                                          | otherwise              -> UnsetPred)
         , (PrefixPred Prefix66      , if | hasPrefix66            -> SetPred
                                          | otherwise              -> UnsetPred)
         , (PrefixPred Prefix67      , if | hasPrefix67            -> SetPred
                                          | otherwise              -> UnsetPred)
         , (PrefixPred PrefixW       , if | opcodeW oc             -> SetPred
                                          | otherwise              -> UnsetPred)
         , (PrefixPred PrefixL       , case opcodeL oc of
                                          Nothing    -> UndefPred
                                          Just True  -> SetPred
                                          Just False -> UnsetPred)
         , (InsnPred Default64OpSize , if | hasDefaultOp64         -> SetPred
                                          | otherwise              -> UnsetPred)
         , (InsnPred Force8bit       , if | hasForce8bit           -> SetPred
                                          | otherwise              -> UnsetPred)
         , (EncodingPred PRexEncoding, case oc of
               (OpLegacy _ (Just _) _ _) -> SetPred
               (OpLegacy _ Nothing _ _)  -> UnsetPred
               _                         -> UndefPred)
         , (EncodingPred (case oc of
               OpLegacy {} -> PLegacyEncoding
               OpVex    {} -> PVexEncoding
               OpXop    {} -> PXopEncoding), SetPred)
         ]


      readParam spec = case opType spec of
         -- One of the two types (for ModRM.rm)
         TME r m -> case modField <$> modrm of
            Just 0b11 -> readParam (spec { opType = r })
            Just _    -> readParam (spec { opType = m })
            Nothing   -> fail "Cannot read ModRM.mod"
         
         -- One of the two types depending on Vex.L
         TLE l128 l256 -> case opcodeL oc of
            Just False -> readParam (spec { opType = l128 })
            Just True  -> readParam (spec { opType = l256 })
            Nothing    -> fail "Cannot read VEX.L/XOP.L"

         -- One of the two types depending on Rex.W
         TWE now w     -> if hasRexW
                              then readParam (spec { opType = w })
                              else readParam (spec { opType = now })
         
         -- Memory address
         T_Mem mtype -> case modrm of
            Nothing     -> case mtype of
                  MemDSrSI -> return $ OpMem mtype $ Addr R_DS rSI Nothing Nothing Nothing
                  MemDSrDI -> return $ OpMem mtype $ Addr seg' rDI Nothing Nothing Nothing
                  MemESrDI -> return $ OpMem mtype $ Addr R_ES rDI Nothing Nothing Nothing
                  _        -> fail "ModRM required"
               where
                  seg' = fromMaybe R_DS segOverride
                  rSI  = case operandSize of
                     OpSize8  -> Just R_SI
                     OpSize16 -> Just R_SI
                     OpSize32 -> Just R_ESI
                     OpSize64 -> Just R_RSI
                  rDI  = case operandSize of
                     OpSize8  -> Just R_DI -- happens in SCAS
                     OpSize16 -> Just R_DI
                     OpSize32 -> Just R_EDI
                     OpSize64 -> Just R_RDI
            Just modrm' -> return $ OpMem mtype $ Addr seg' base idx scl disp
               where
                  toR = case addressSize of
                           AddrSize32 -> gpr 32
                           AddrSize64 -> gpr 64
                           AddrSize16 -> error "Trying to use AddrSize16"
                  base = if addressSize == AddrSize16
                           then case (modField modrm', rmField modrm') of
                              (_,    0b000) -> Just R_BX
                              (_,    0b001) -> Just R_BX
                              (_,    0b010) -> Just R_BP
                              (_,    0b011) -> Just R_BP
                              (_,    0b100) -> Nothing    
                              (_,    0b101) -> Nothing    
                              (0b00, 0b110) -> Nothing    
                              (_,    0b110) -> Just R_BP
                              (_,    0b111) -> Just R_BX
                              _             -> error "Invalid 16-bit addressing"
                           else case (modField modrm', rmField modrm') of
                              (0b00, 0b101) -> if is64bitMode'
                                                   then Just R_RIP
                                                   else Nothing
                              -- SIB: if mod is 0b00, don't use EBP as base.
                              (0b00, 0b100)
                                 | baseField sib' == 0b101 -> Nothing
                              (_,    0b100) -> Just (toR (fromIntegral sibBase))
                              _             -> Just (toR (fromIntegral modRMrm))
                  idx = if addressSize == AddrSize16
                           then case rmField modrm' of
                              0b000 -> Just R_SI
                              0b001 -> Just R_DI
                              0b010 -> Just R_SI
                              0b011 -> Just R_DI
                              0b100 -> Just R_SI
                              0b101 -> Just R_DI
                              0b110 -> Nothing
                              0b111 -> Nothing
                              _     -> error "Invalid 16-bit addressing"
                        else case (rmField modrm', indexField sib') of
                           -- SIB: if index is 0b100 (should be ESP), don't
                           -- use any index
                           (0b100, 0b100) -> Nothing
                           (0b100, _    ) -> Just (toR (fromIntegral sibIdx))
                           _              -> Nothing -- no SIB
                  scl = if addressSize /= AddrSize16 && rmField modrm' == 0b100
                           then Just (scaleField sib')
                           else Nothing
                  seg' = fromMaybe (defaultSegment base) segOverride
                        
         -- Register
         T_Reg rfam -> return <| OpReg
                              <| regFixupTermFamily updatedFam
            where
               fam' = case reducePredicates oracle rfam of
                  Match x -> x
                  r -> error ("Cannot reduce register family to a terminal: " ++ show r)

               -- get raw register id
               rawId = fromIntegral $ case opEnc spec of
                  RM         -> modRMrm
                  Reg        -> modRMreg
                  Vvvv       -> vvvv
                  OpcodeLow3 -> opcodeRegId
                  Implicit   -> case regFamId fam' of
                     Singleton i -> fromIntegral i
                     e           -> error ("Invalid implicit register id: " ++ show e)
                  e          -> error ("Invalid register encoding: " ++ show e)

               -- update family id and offset
               updatedFam = fam'
                  { regFamId     = trySetCSet regId     (regFamId fam')
                  , regFamOffset = trySetCSet regOffset (regFamOffset fam')
                  }

               -- get the adjusted register id and the offset
               (regId,regOffset) = case fam' of
                  -- Check if we are left with a GPR of size 8 whose offset
                  -- is OneOf[0,8]. Fix it and fix the id accordingly.
                  RegFam
                     (Singleton GPR)
                     _
                     (Singleton 8)
                     (OneOf [0,8])
                       | not useExtRegs && 4 <= rawId && rawId <= 7 -> (rawId-4, 8)
                  _ -> (rawId,0)



         -- Sub-part of a register
         T_SubReg _ rtype -> readParam (spec {opType = T_Reg rtype})

         -- Pair (AAA:BBB)
         T_Pair (T_Reg f1) (T_Reg f2) ->
            -- registers are constant in pairs, so we just have to fix them. We
            -- allow the first register to not be fixable, in which case it's
            -- not a pair. We do this to support the family:
            --    AX, DX:AX, EDX:RAX, RDX:RAX
            return $ case (regFixupPredFamilyMaybe oracle f1, regFixupPredFamily oracle f2) of
               (Just r1,r2) -> OpRegPair r1 r2
               (Nothing,r)  -> OpReg r

         T_Pair (T_Imm ImmSize16) (T_Imm ImmSizeOp) -> return $ case imms of
            [SizedValue16 x, SizedValue16 y] -> OpPtr16_16 x y
            [SizedValue16 x, SizedValue32 y] -> OpPtr16_32 x y
            xs -> error ("Invalid immediate operands for ptr16x: " ++ show xs)

         T_Pair (T_Imm ImmSize16) (T_Imm ImmSize8) -> return $ case imms of
            [SizedValue16 x, SizedValue8 y] -> OpStackFrame x y
            xs -> error ("Invalid immediate operands for ENTER: " ++ show xs)

         T_Pair x y -> error ("Unhandled operand pair: " ++ show (x,y))

         -- Immediate
         T_Imm (ImmConst n) ->
            -- 8-bit is currently enough for known instructions
            return (OpImmediate (SizedValue8 (fromIntegral n)))

         T_Imm _            -> case imms of
            [x] -> return (OpImmediate x)
            xs  -> error ("Invalid immediate: " ++ show xs)

         -- IP relative offset
         T_Rel _ -> return (OpCodeAddr Addr
            { addrSeg   = R_CS
            , addrBase  = Just rIP
            , addrIndex = Nothing
            , addrScale = Nothing
            , addrDisp  = disp
            })
         
         -- Segment relative offset
         T_MemOffset -> return (OpCodeAddr Addr
            { addrSeg   = seg
            , addrBase  = Nothing
            , addrIndex = Nothing
            , addrScale = Nothing
            , addrDisp  = disp
            })

         -- DS:EAX or DS:RAX (used by monitor)
         T_MemDSrAX -> return (OpMem MemVoid Addr
            { addrSeg   = R_DS
            , addrBase  = Just $ if is64bitMode' then R_RAX else R_EAX
            , addrIndex = Nothing
            , addrScale = Nothing
            , addrDisp  = disp
            })

      -- The default segment is DS except
      --  * for some instruction (cf DefaultSegment property)
      --  * if rBP or rSP is used as base (in which case it is SS)
      --  * for string instructions' source operand (in which case it is ES)
      defSeg = case filter isD (encProperties enc) of
            []                 -> R_DS
            [DefaultSegment s] -> s
            _                  -> error "More than one default segment"
         where
            isD (DefaultSegment _) = True
            isD _                  = False

      -- segment override prefixes
      segOverride = case filter ((== 3) . legacyPrefixGroup) ps of
         []               -> Nothing
         [LegacyPrefix2E] -> Just R_CS
         [LegacyPrefix3E] -> Just R_DS
         [LegacyPrefix26] -> Just R_ES
         [LegacyPrefix64] -> Just R_FS
         [LegacyPrefix65] -> Just R_GS
         [LegacyPrefix36] -> Just R_SS
         xs -> error ("More than one segment-override prefix: "++show xs)

      -- memory segment (when override is allowed)
      seg = fromMaybe defSeg segOverride

      rIP = case addressSize of
               AddrSize16 -> R_IP
               AddrSize32 -> R_EIP
               AddrSize64 -> R_RIP

      gpr sz r  = regGPR useExtRegs sz r

      -- extended ModRM.reg (with REX.R, VEX.R, etc.)
      modRMreg = opcodeR oc `unsafeShiftL` 3 .|. regField modrm'
         where modrm' = fromMaybe (error "Cannot read ModRM") modrm
            
      -- | Extended ModRM.rm (with REX.B, VEX.B, etc.)
      modRMrm = opcodeB oc `unsafeShiftL` 3 .|. rmField modrm'
         where modrm' = fromMaybe (error "Cannot read ModRM") modrm

      sib' = fromJust sib

      -- | Extended SIB index (with REX.X, VEX.X, etc.)
      sibIdx = opcodeX oc `unsafeShiftL` 3 .|. indexField sib'
            
      -- | Extended SIB base (with REX.B, VEX.B, etc.)
      sibBase = opcodeB oc `unsafeShiftL` 3 .|. baseField sib'

      -- | Extended register id in opcode (with REX.B, VEX.B, etc.)
      opcodeRegId =  opcodeB oc `unsafeShiftL` 3 .|. (opcodeByte oc .&. 0b111)

      -- VVVV field
      vvvv = case oc of
         OpLegacy {} -> error "Trying to read Vvvv with legacy opcode"
         OpVex v _   -> vexVVVV v
         OpXop v _   -> vexVVVV v

      
      -- when a REX prefix is used, some 8-bit registers cannot be encoded
      useExtRegs = case oc of
         OpLegacy _ Nothing _ _  -> False
         OpLegacy _ (Just _) _ _ -> True
         _ -> error ("useExtRegs: we shouldn't check for 8-bit registers with non-legacy opcode: " ++ show oc)

         
   ops' <- forM (encOperands enc) readParam

   -- reverse operands (FPU dest, reversable bit)
   let ops = case (encReversableBit enc, encFPUDestBit enc) of
               (Just b, Nothing)
                  | testBit (opcodeByte oc) b -> reverse ops'
               (Nothing, Just b)
                  | testBit (opcodeByte oc) b -> reverse ops'
               _                              -> ops'

   return ops


-- | Give the default segment for the given base register
defaultSegment :: Maybe Register -> Register
defaultSegment = \case
   Just R_BP  -> R_SS
   Just R_SP  -> R_SS
   Just R_EBP -> R_SS
   Just R_ESP -> R_SS
   Just R_RBP -> R_SS
   Just R_RSP -> R_SS
   _          -> R_DS
