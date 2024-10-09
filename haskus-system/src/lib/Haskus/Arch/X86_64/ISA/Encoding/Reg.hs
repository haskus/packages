{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-}
module Haskus.Arch.X86_64.ISA.Encoding.Reg
  ( Reg
  , regCode
  , regCodeX
  , regSize
  , regREX
  , regNO_REX
  , compatibleRegs
  , isReg8
  , isReg16
  , isReg32
  , isReg64
  -- * Registers
  , pattern R_AL,   pattern R_AX,   pattern R_EAX,  pattern R_RAX
  , pattern R_BL,   pattern R_BX,   pattern R_EBX,  pattern R_RBX
  , pattern R_CL,   pattern R_CX,   pattern R_ECX,  pattern R_RCX
  , pattern R_DL,   pattern R_DX,   pattern R_EDX,  pattern R_RDX
  , pattern R_SIL,  pattern R_SI,   pattern R_ESI,  pattern R_RSI
  , pattern R_DIL,  pattern R_DI,   pattern R_EDI,  pattern R_RDI
  , pattern R_BPL,  pattern R_BP,   pattern R_EBP,  pattern R_RBP
  , pattern R_SPL,  pattern R_SP,   pattern R_ESP,  pattern R_RSP
  , pattern R_AH,   pattern R_BH,   pattern R_CH,   pattern R_DH
  , pattern R_R8B,  pattern R_R8W,  pattern R_R8D,  pattern R_R8
  , pattern R_R9B,  pattern R_R9W,  pattern R_R9D,  pattern R_R9
  , pattern R_R10B, pattern R_R10W, pattern R_R10D, pattern R_R10
  , pattern R_R11B, pattern R_R11W, pattern R_R11D, pattern R_R11
  , pattern R_R12B, pattern R_R12W, pattern R_R12D, pattern R_R12
  , pattern R_R13B, pattern R_R13W, pattern R_R13D, pattern R_R13
  , pattern R_R14B, pattern R_R14W, pattern R_R14D, pattern R_R14
  , pattern R_R15B, pattern R_R15W, pattern R_R15D, pattern R_R15
  -- * Just patterns
  , pattern JR_AL,   pattern JR_AX,   pattern JR_EAX,  pattern JR_RAX
  , pattern JR_BL,   pattern JR_BX,   pattern JR_EBX,  pattern JR_RBX
  , pattern JR_CL,   pattern JR_CX,   pattern JR_ECX,  pattern JR_RCX
  , pattern JR_DL,   pattern JR_DX,   pattern JR_EDX,  pattern JR_RDX
  , pattern JR_SIL,  pattern JR_SI,   pattern JR_ESI,  pattern JR_RSI
  , pattern JR_DIL,  pattern JR_DI,   pattern JR_EDI,  pattern JR_RDI
  , pattern JR_BPL,  pattern JR_BP,   pattern JR_EBP,  pattern JR_RBP
  , pattern JR_SPL,  pattern JR_SP,   pattern JR_ESP,  pattern JR_RSP
  , pattern JR_AH,   pattern JR_BH,   pattern JR_CH,   pattern JR_DH
  , pattern JR_R8B,  pattern JR_R8W,  pattern JR_R8D,  pattern JR_R8
  , pattern JR_R9B,  pattern JR_R9W,  pattern JR_R9D,  pattern JR_R9
  , pattern JR_R10B, pattern JR_R10W, pattern JR_R10D, pattern JR_R10
  , pattern JR_R11B, pattern JR_R11W, pattern JR_R11D, pattern JR_R11
  , pattern JR_R12B, pattern JR_R12W, pattern JR_R12D, pattern JR_R12
  , pattern JR_R13B, pattern JR_R13W, pattern JR_R13D, pattern JR_R13
  , pattern JR_R14B, pattern JR_R14W, pattern JR_R14D, pattern JR_R14
  , pattern JR_R15B, pattern JR_R15W, pattern JR_R15D, pattern JR_R15
  )
where

import Haskus.Binary.Word
import Haskus.Binary.Bits
import Haskus.Arch.X86_64.ISA.Size

-- | General-purpose registers
--
-- Encoded as:
--  6-7: size 00=8, 01=16, 10=32, 11=64
--    5: NO_REX: require that REX isn't present (AH, BH, CH, DH registers)
--    4: REX: require REX to be present even if bit 3 of code is 0 (extended registers)
--  3-0: register code
newtype Reg
  = Reg U8
  deriving (Eq,Ord)

regCode :: Reg -> U8
regCode (Reg w) = w .&. 0b00_0_0_1111

-- | Code with the MSB extracted to be put in REX.X for example
regCodeX :: Reg -> (Bool, U8)
regCodeX r = (testBit c 3, c .&. 0b0111)
  where
    !c = regCode r

regREX :: Reg -> Bool
regREX (Reg w) = w .&. 0b00_0_1_0000 /= 0

regNO_REX :: Reg -> Bool
regNO_REX (Reg w) = w .&. 0b00_1_0_0000 /= 0

regSize :: Reg -> OperandSize
regSize (Reg w) = case w `shiftR` 6 of
  0b00 -> OpSize8
  0b01 -> OpSize16
  0b10 -> OpSize32
  _    -> OpSize64

isReg8 :: Reg -> Maybe Reg
isReg8 r = if regSize r == OpSize8 then Just r else Nothing

isReg16 :: Reg -> Maybe Reg
isReg16 r = if regSize r == OpSize16 then Just r else Nothing

isReg32 :: Reg -> Maybe Reg
isReg32 r = if regSize r == OpSize32 then Just r else Nothing

isReg64 :: Reg -> Maybe Reg
isReg64 r = if regSize r == OpSize64 then Just r else Nothing

-- | Are two registers compatible (encodable in the same instruction)
compatibleRegs :: Reg -> Reg -> Bool
compatibleRegs (Reg a) (Reg b) = not (regREX r && regNO_REX r)
  where
    !r = Reg (a .|. b)



pattern R_AL  = Reg 0b00_0_0_0000
pattern R_AX  = Reg 0b01_0_0_0000
pattern R_EAX = Reg 0b10_0_0_0000
pattern R_RAX = Reg 0b11_0_0_0000

pattern R_BL  = Reg 0b00_0_0_0011
pattern R_BX  = Reg 0b01_0_0_0011
pattern R_EBX = Reg 0b10_0_0_0011
pattern R_RBX = Reg 0b11_0_0_0011

pattern R_CL  = Reg 0b00_0_0_0001
pattern R_CX  = Reg 0b01_0_0_0001
pattern R_ECX = Reg 0b10_0_0_0001
pattern R_RCX = Reg 0b11_0_0_0001

pattern R_DL  = Reg 0b00_0_0_0010
pattern R_DX  = Reg 0b01_0_0_0010
pattern R_EDX = Reg 0b10_0_0_0010
pattern R_RDX = Reg 0b11_0_0_0010

pattern R_SIL = Reg 0b00_0_1_0110
pattern R_SI  = Reg 0b01_0_0_0110
pattern R_ESI = Reg 0b10_0_0_0110
pattern R_RSI = Reg 0b11_0_0_0110

pattern R_DIL = Reg 0b00_0_1_0111
pattern R_DI  = Reg 0b01_0_0_0111
pattern R_EDI = Reg 0b10_0_0_0111
pattern R_RDI = Reg 0b11_0_0_0111

pattern R_BPL = Reg 0b00_0_1_0101
pattern R_BP  = Reg 0b01_0_0_0101
pattern R_EBP = Reg 0b10_0_0_0101
pattern R_RBP = Reg 0b11_0_0_0101

pattern R_SPL = Reg 0b00_0_1_0100
pattern R_SP  = Reg 0b01_0_0_0100
pattern R_ESP = Reg 0b10_0_0_0100
pattern R_RSP = Reg 0b11_0_0_0100

-- legacy uppeer 8-bit registers
pattern R_AH = Reg 0b00_1_0_0000
pattern R_BH = Reg 0b00_1_0_0011
pattern R_CH = Reg 0b00_1_0_0001
pattern R_DH = Reg 0b00_1_0_0010

pattern R_R8B = Reg 0b00_0_1_1000
pattern R_R8W = Reg 0b01_0_1_1000
pattern R_R8D = Reg 0b10_0_1_1000
pattern R_R8  = Reg 0b11_0_1_1000

pattern R_R9B = Reg 0b00_0_1_1001
pattern R_R9W = Reg 0b01_0_1_1001
pattern R_R9D = Reg 0b10_0_1_1001
pattern R_R9  = Reg 0b11_0_1_1001

pattern R_R10B = Reg 0b00_0_1_1010
pattern R_R10W = Reg 0b01_0_1_1010
pattern R_R10D = Reg 0b10_0_1_1010
pattern R_R10  = Reg 0b11_0_1_1010

pattern R_R11B = Reg 0b00_0_1_1011
pattern R_R11W = Reg 0b01_0_1_1011
pattern R_R11D = Reg 0b10_0_1_1011
pattern R_R11  = Reg 0b11_0_1_1011

pattern R_R12B = Reg 0b00_0_1_1100
pattern R_R12W = Reg 0b01_0_1_1100
pattern R_R12D = Reg 0b10_0_1_1100
pattern R_R12  = Reg 0b11_0_1_1100

pattern R_R13B = Reg 0b00_0_1_1101
pattern R_R13W = Reg 0b01_0_1_1101
pattern R_R13D = Reg 0b10_0_1_1101
pattern R_R13  = Reg 0b11_0_1_1101

pattern R_R14B = Reg 0b00_0_1_1110
pattern R_R14W = Reg 0b01_0_1_1110
pattern R_R14D = Reg 0b10_0_1_1110
pattern R_R14  = Reg 0b11_0_1_1110

pattern R_R15B = Reg 0b00_0_1_1111
pattern R_R15W = Reg 0b01_0_1_1111
pattern R_R15D = Reg 0b10_0_1_1111
pattern R_R15  = Reg 0b11_0_1_1111

{-# COMPLETE R_AL, R_AX, R_EAX, R_RAX,
             R_BL, R_BX, R_EBX, R_RBX,
             R_CL, R_CX, R_ECX, R_RCX,
             R_DL, R_DX, R_EDX, R_RDX,
             R_SIL, R_SI, R_ESI, R_RSI,
             R_DIL, R_DI, R_EDI, R_RDI,
             R_BPL, R_BP, R_EBP, R_RBP,
             R_SPL, R_SP, R_ESP, R_RSP,
             R_AH, R_BH, R_CH, R_DH,
             R_R8B, R_R8W, R_R8D, R_R8,
             R_R9B, R_R9W, R_R9D, R_R9,
             R_R10B, R_R10W, R_R10D, R_R10,
             R_R11B, R_R11W, R_R11D, R_R11,
             R_R12B, R_R12W, R_R12D, R_R12,
             R_R13B, R_R13W, R_R13D, R_R13,
             R_R14B, R_R14W, R_R14D, R_R14,
             R_R15B, R_R15W, R_R15D, R_R15 #-}

instance Show Reg where
  show = \case
    R_AL    -> "AL"
    R_AX    -> "AX"
    R_EAX   -> "EAX"
    R_RAX   -> "RAX"
    R_BL    -> "BL"
    R_BX    -> "BX"
    R_EBX   -> "EBX"
    R_RBX   -> "EBX"
    R_CL    -> "CL"
    R_CX    -> "CX"
    R_ECX   -> "ECX"
    R_RCX   -> "ECX"
    R_DL    -> "DL"
    R_DX    -> "DX"
    R_EDX   -> "EDX"
    R_RDX   -> "EDX"
    R_SIL   -> "SIL"
    R_SI    -> "SI"
    R_ESI   -> "ESI"
    R_RSI   -> "RSI"
    R_DIL   -> "DIL"
    R_DI    -> "DI"
    R_EDI   -> "EDI"
    R_RDI   -> "RDI"
    R_BPL   -> "BPL"
    R_BP    -> "BP"
    R_EBP   -> "EBP"
    R_RBP   -> "RBP"
    R_SPL   -> "SPL"
    R_SP    -> "SP"
    R_ESP   -> "ESP"
    R_RSP   -> "RSP"
    R_AH    -> "AH"
    R_BH    -> "BH"
    R_CH    -> "CH"
    R_DH    -> "DH"
    R_R8B   -> "R8B"
    R_R8W   -> "R8W"
    R_R8D   -> "R8D"
    R_R8    -> "R8"
    R_R9B   -> "R9B"
    R_R9W   -> "R9W"
    R_R9D   -> "R9D"
    R_R9    -> "R9"
    R_R10B  -> "R10B"
    R_R10W  -> "R10W"
    R_R10D  -> "R10D"
    R_R10   -> "R10"
    R_R11B  -> "R11B"
    R_R11W  -> "R11W"
    R_R11D  -> "R11D"
    R_R11   -> "R11"
    R_R12B  -> "R12B"
    R_R12W  -> "R12W"
    R_R12D  -> "R12D"
    R_R12   -> "R12"
    R_R13B  -> "R13B"
    R_R13W  -> "R13W"
    R_R13D  -> "R13D"
    R_R13   -> "R13"
    R_R14B  -> "R14B"
    R_R14W  -> "R14W"
    R_R14D  -> "R14D"
    R_R14   -> "R14"
    R_R15B  -> "R15B"
    R_R15W  -> "R15W"
    R_R15D  -> "R15D"
    R_R15   -> "R15"

pattern JR_AL  = Just R_AL
pattern JR_AX  = Just R_AX
pattern JR_EAX = Just R_EAX
pattern JR_RAX = Just R_RAX

pattern JR_BL  = Just R_BL  
pattern JR_BX  = Just R_BX  
pattern JR_EBX = Just R_EBX 
pattern JR_RBX = Just R_RBX 
                      
pattern JR_CL  = Just R_CL  
pattern JR_CX  = Just R_CX  
pattern JR_ECX = Just R_ECX 
pattern JR_RCX = Just R_RCX 
                      
pattern JR_DL  = Just R_DL  
pattern JR_DX  = Just R_DX  
pattern JR_EDX = Just R_EDX 
pattern JR_RDX = Just R_RDX 
                      
pattern JR_SIL = Just R_SIL 
pattern JR_SI  = Just R_SI  
pattern JR_ESI = Just R_ESI 
pattern JR_RSI = Just R_RSI 
                      
pattern JR_DIL = Just R_DIL 
pattern JR_DI  = Just R_DI  
pattern JR_EDI = Just R_EDI 
pattern JR_RDI = Just R_RDI 
                      
pattern JR_BPL = Just R_BPL 
pattern JR_BP  = Just R_BP  
pattern JR_EBP = Just R_EBP 
pattern JR_RBP = Just R_RBP 
                      
pattern JR_SPL = Just R_SPL 
pattern JR_SP  = Just R_SP  
pattern JR_ESP = Just R_ESP 
pattern JR_RSP = Just R_RSP 

-- legacy uppeer 8-bit registers
pattern JR_AH = Just R_AH 
pattern JR_BH = Just R_BH 
pattern JR_CH = Just R_CH 
pattern JR_DH = Just R_DH 

pattern JR_R8B = Just R_R8B 
pattern JR_R8W = Just R_R8W 
pattern JR_R8D = Just R_R8D 
pattern JR_R8  = Just R_R8  
                      
pattern JR_R9B = Just R_R9B 
pattern JR_R9W = Just R_R9W 
pattern JR_R9D = Just R_R9D 
pattern JR_R9  = Just R_R9  

pattern JR_R10B = Just R_R10B 
pattern JR_R10W = Just R_R10W 
pattern JR_R10D = Just R_R10D 
pattern JR_R10  = Just R_R10  
                        
pattern JR_R11B = Just R_R11B 
pattern JR_R11W = Just R_R11W 
pattern JR_R11D = Just R_R11D 
pattern JR_R11  = Just R_R11  
                        
pattern JR_R12B = Just R_R12B 
pattern JR_R12W = Just R_R12W 
pattern JR_R12D = Just R_R12D 
pattern JR_R12  = Just R_R12  
                        
pattern JR_R13B = Just R_R13B 
pattern JR_R13W = Just R_R13W 
pattern JR_R13D = Just R_R13D 
pattern JR_R13  = Just R_R13  
                        
pattern JR_R14B = Just R_R14B 
pattern JR_R14W = Just R_R14W 
pattern JR_R14D = Just R_R14D 
pattern JR_R14  = Just R_R14  
                        
pattern JR_R15B = Just R_R15B 
pattern JR_R15W = Just R_R15W 
pattern JR_R15D = Just R_R15D 
pattern JR_R15  = Just R_R15  
