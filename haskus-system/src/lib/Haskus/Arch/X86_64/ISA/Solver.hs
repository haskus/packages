{-# LANGUAGE LambdaCase #-}

-- | Constraint solver
module Haskus.Arch.X86_64.ISA.Solver
   ( X86Rule
   , X86Constraint
   -- * Predicates
   , X86Pred (..)
   , X86Err
   , ContextPred (..)
   , InsnPred (..)
   , PrefixPred (..)
   , EncodingPred (..)
   , X86PredOracle
   , PredError (..)
   , makeOracleX86
   , checkOracle
   , pPrefix
   , pNotPrefix
   , sPrefix
   , sPrefixDefault
   , pRegModRM
   , sRegModRM
   , pMode
   , isModePredicate
   , pMode64bit
   , pCS_D
   , pLegacy8bitRegs
   , pOverriddenOperationSize64
   , pOverriddenOperationSize
   , pForce8bit
   , pFPUSizeBit
   , pSignExtendBit
   , pOverriddenAddressSize
   , pOpSize64
   -- * Rules
   , rDefaultOperationSize
   , rDefaultAddressSize
   , rOverriddenOperationSize
   , rOverriddenOperationSize64
   , rOverriddenAddressSize
   )
where

import Haskus.Arch.X86_64.ISA.Size
import Haskus.Arch.X86_64.ISA.Context
import Haskus.Utils.Solver
import Haskus.Utils.Maybe
import qualified Haskus.Utils.List as List

-- | Context predicate
data ContextPred
   = Mode Mode    -- ^ Execution mode
   | CS_D         -- ^ CS.D segment descriptor flag
   | SS_B         -- ^ SS.B segment descriptor flag
   deriving (Show,Eq,Ord)

-- | Prefix predicates
data PrefixPred
   = Prefix66     -- ^ 0x66
   | Prefix67     -- ^ 0x67
   | PrefixW      -- ^ W bit
   | PrefixL      -- ^ L bit
   deriving (Show,Eq,Ord)

-- | Instruction specific predicates
data InsnPred
   = Default64OpSize    -- ^ Instruction defaulting to 64-bit size
   | RegModRM           -- ^ Require ModRM.mod == 11b (register)
   | Force8bit          -- ^ Set Force8Bit bit in the opcode
   | FPUSizeBit         -- ^ Set Size bit in the FPU opcode
   | SignExtendBit      -- ^ Sign-extend opcode bit set
   deriving (Show,Eq,Ord)

-- | Encoding predicates
data EncodingPred
   = PLegacyEncoding -- ^ Legacy encoding
   | PRexEncoding    -- ^ Legacy encoding with REX prefix
   | PVexEncoding    -- ^ VEX encoding
   | PXopEncoding    -- ^ XOP encoding
   | PEvexEncoding   -- ^ EVEX encoding
   | PMvexEncoding   -- ^ MVEX encoding
   deriving (Show,Eq,Ord,Enum)

-- | All the predicats
data X86Pred
   = ContextPred ContextPred
   | PrefixPred PrefixPred
   | EncodingPred EncodingPred
   | InsnPred InsnPred
   deriving (Show,Eq,Ord)

type X86Err        = String
type X86Rule a     = Rule X86Err X86Pred a
type X86Constraint = Constraint X86Err X86Pred

-----------------------------------------------------
-- Predicates
-----------------------------------------------------

-- | Predicate oracle
type X86PredOracle = PredOracle X86Pred

data PredError
   = PredImply [(X86Pred,PredState)] [(X86Pred,PredState)]
   | PredIncompatible [(X86Pred,PredState)]
   deriving (Show)

-- | Implied constraints on x86 arch
--
-- For each tuple (cs,rs), if all the constraints in cs match, then all the
-- constraints in rs must match.
oracleImplications :: [([(X86Pred,PredState)],[(X86Pred,PredState)])]
oracleImplications =
   -- modes are mutually exclusive
   [ ( [ (ContextPred (Mode m) , SetPred) ]
     , [ (ContextPred (Mode m'), UnsetPred) | m' <- ms ]
     )
   | (m,ms) <- List.pick1 allModes
   ]
   ++
   [
     -- CS.D doesn't make sense in real-mode and virtual 8086 mode
      (  [ (ContextPred (Mode Mode16)   , SetPred)]
      ,  [ (ContextPred CS_D            , InvalidPred)]
      )
   ,  (  [ (ContextPred (Mode Mode32_16), SetPred)]
      ,  [ (ContextPred CS_D            , InvalidPred)]
      )

   , -- CS.D can't be 1 in long 64-bit mode.
      (  [ (ContextPred (Mode Mode64), SetPred)]
      ,  [ (ContextPred CS_D         , UnsetPred)]
      )

   ,  -- REX prefix only valid with a legacy encoding
      (  [(EncodingPred PRexEncoding   , SetPred)]
      ,  [(EncodingPred PLegacyEncoding, SetPred)]
      )

   , -- W doesn't make sense in real-mode and virtual 8086 mode
      ( [ (PrefixPred PrefixW          , SetPred)]
      , [ (ContextPred (Mode Mode16)   , UnsetPred)]
      )
   ,  ( [ (PrefixPred PrefixW          , SetPred)]
      , [ (ContextPred (Mode Mode32_16), UnsetPred)]
      )
   ,  ( [ (ContextPred (Mode Mode16)   , SetPred)]
      , [ (PrefixPred PrefixW          , InvalidPred)]
      )
   ,  ( [ (ContextPred (Mode Mode32_16), SetPred)]
      , [ (PrefixPred PrefixW          , InvalidPred)]
      )

   , -- L doesn't make sense in real-mode and virtual 8086 mode
      ( [ (PrefixPred PrefixL          , SetPred)]
      , [ (ContextPred (Mode Mode16)   , UnsetPred)]
      )
   ,  ( [ (PrefixPred PrefixL          , SetPred)]
      , [ (ContextPred (Mode Mode32_16), UnsetPred)]
      )
   ,  ( [ (ContextPred (Mode Mode16)   , SetPred)]
      , [ (PrefixPred PrefixL          , InvalidPred)]
      )
   ,  ( [ (ContextPred (Mode Mode32_16), SetPred)]
      , [ (PrefixPred PrefixL          , InvalidPred)]
      )

   ]


-- | Make an oracle for the X86. Add implied constraints
makeOracleX86 :: [(X86Pred,PredState)] -> X86PredOracle
makeOracleX86 xs = go (makeOracle xs) oracleImplications
   where
      -- test matching constraints and add implied constraints
      go oracle []           = oracle
      go oracle ((cs,rs):is)
         | predAll oracle cs = go (predAdd rs oracle) is
         | otherwise         = go oracle is


-- | List of incompatible predicates
oracleIncompatibilities :: [[(X86Pred, PredState)]]
oracleIncompatibilities =
      -- all modes can't be unset
      [[(ContextPred (Mode m),UnsetPred) | m <- allModes]]

      -- encodings are incompatible
      ++ exclusive (fmap EncodingPred encodings)

      -- W isn't invalid in 64-bit/protected/compat modes
      ++ [[(PrefixPred PrefixW, InvalidPred)
          ,(ContextPred (Mode Mode64), SetPred)
          ]
         ,[(PrefixPred PrefixW, InvalidPred)
          ,(ContextPred (Mode Mode64_32), SetPred)
          ]
         ,[(PrefixPred PrefixW, InvalidPred)
          ,(ContextPred (Mode Mode32), SetPred)
          ]
         ]


   where
      exclusive []     = []
      exclusive [_]    = []
      exclusive (x:xs) = [[(x,SetPred),(y,SetPred)] | y <- xs] ++ exclusive xs

      encodings =   [ PLegacyEncoding
                    -- , PRexEncoding
                    , PVexEncoding
                    , PXopEncoding
                    , PEvexEncoding
                    , PMvexEncoding
                    ]


predAll :: X86PredOracle -> [(X86Pred,PredState)] -> Bool
predAll oracle is = all (\(p,s) -> predIs oracle p s) is

-- | Check an oracle, return a list of incompatible predicates
checkOracle :: Bool -> X86PredOracle -> [PredError]
checkOracle strict oracle =
      (fmap PredIncompatible (mapMaybe checkCompat oracleIncompatibilities))
      ++ (fmap (uncurry PredImply) (mapMaybe checkImply oracleImplications))
   where
      predAllOrUndef is = all (\(p,s) -> case (s,predState oracle p) of
                                          (UndefPred,_)
                                             | not strict -> True
                                          (_,UndefPred)   -> True
                                          (_,s')          -> s == s'
                             ) is

      checkCompat is = 
         if predAll oracle is
            then Just is
            else Nothing

      checkImply (cs,rs) =
         if predAll oracle cs && not (predAllOrUndef rs)
            then Just (cs,rs)
            else Nothing



-- | Allow the use of legacy 8-bit AH,BH,CH,DH registers
pLegacy8bitRegs :: X86Constraint
pLegacy8bitRegs = And [ Predicate (EncodingPred PLegacyEncoding)
                      , Not (Predicate (EncodingPred PRexEncoding))
                      ]

-- | 64-bit long mode predicate
pMode64bit :: X86Constraint
pMode64bit = pMode Mode64

-- | Exclusive mode predicate
pMode :: Mode -> X86Constraint
pMode = Predicate . ContextPred . Mode

-- | Check if a predicate is a Mode predicate
isModePredicate :: X86Pred -> Bool
isModePredicate (ContextPred (Mode _)) = True
isModePredicate _ = False

-- | CS.D flag
pCS_D :: X86Constraint
pCS_D = Predicate (ContextPred CS_D)

-- | Prefix predicate
pPrefix :: PrefixPred -> X86Constraint
pPrefix = Predicate . PrefixPred

-- | Prefix absence predicate
pNotPrefix :: PrefixPred -> X86Constraint
pNotPrefix = Not . pPrefix

-- | Select using a prefix
sPrefix :: PrefixPred -> X86Rule a -> X86Rule a -> X86Rule a
sPrefix p a b = OrderedNonTerminal
   [ (pNotPrefix p, a)
   , (pPrefix p   , b)
   ]

-- | Select using a prefix and default value for invalid prefix predicate
sPrefixDefault :: PrefixPred -> X86Rule a -> X86Rule a -> X86Rule a -> X86Rule a
sPrefixDefault p d a b = OrderedNonTerminal
   [ (Not (IsValid (PrefixPred p)), d)
   , (pNotPrefix p, a)
   , (pPrefix p   , b)
   ]

-- | ModRM.mod = 11 predicate
pRegModRM :: X86Constraint
pRegModRM = Predicate (InsnPred RegModRM)

-- | Select using a prefix
sRegModRM :: X86Rule a -> X86Rule a -> X86Rule a
sRegModRM a b = OrderedNonTerminal
   [ (Not $ pRegModRM, a)
   , (      pRegModRM, b)
   ]

-- | Overriden 64-bit operation size predicate
pOverriddenOperationSize64 :: OperandSize -> X86Constraint
pOverriddenOperationSize64 t = rOverriddenOperationSize64 `evalsTo` t

-- | Overriden operation size predicate
pOverriddenOperationSize :: OperandSize -> X86Constraint
pOverriddenOperationSize t = rOverriddenOperationSize `evalsTo` t

-- | Force 8-bit operand size
pForce8bit :: X86Constraint
pForce8bit = Predicate (InsnPred Force8bit)

-- | FPU Size opcode bit
pFPUSizeBit :: X86Constraint
pFPUSizeBit = Predicate (InsnPred FPUSizeBit)

-- | Sign-extend opcode bit
pSignExtendBit :: X86Constraint
pSignExtendBit = Predicate (InsnPred SignExtendBit)

-- | Overriden address size predicate
pOverriddenAddressSize :: AddressSize -> X86Constraint
pOverriddenAddressSize t = rOverriddenAddressSize `evalsTo` t


-----------------------------------------------------
-- Predicates
-----------------------------------------------------

-- | Default operation size (DOS)
rDefaultOperationSize :: X86Rule OperandSize
rDefaultOperationSize = NonTerminal
      [ (pMode Mode16   , Terminal OpSize16)
      , (pMode Mode32_16, Terminal OpSize16)
      , (pMode Mode32   , s16o32)
      , (pMode Mode64_32, s16o32)
      , (pMode Mode64   , s32oFail)
      ]
   where
      s16o32   = NonTerminal
                  [ (Not pCS_D, Terminal OpSize16)
                  , (pCS_D    , Terminal OpSize32)
                  ]
      s32oFail = NonTerminal
                  [ (Not pCS_D, Terminal OpSize32)
                  ]

-- | Default address size (DAS)
rDefaultAddressSize :: X86Rule AddressSize
rDefaultAddressSize = NonTerminal
      [ (pMode Mode16   , Terminal AddrSize16)
      , (pMode Mode32_16, Terminal AddrSize16)
      , (pMode Mode32   , s16o32)
      , (pMode Mode64_32, s16o32)
      , (pMode Mode64   , s32oFail)
      ]
   where
      s16o32   = NonTerminal
                  [ (Not pCS_D, Terminal AddrSize16)
                  , (pCS_D    , Terminal AddrSize32)
                  ]
      s32oFail = NonTerminal
                  [ (Not pCS_D, Terminal AddrSize64)
                  ]

-- | Overridden operation size (OOS)
rOverriddenOperationSize :: X86Rule OperandSize
rOverriddenOperationSize = OrderedNonTerminal
      [ (pPrefix Prefix66, NonTerminal p66)
      , (CBool True      , rDefaultOperationSize)
      ]
   where
      p66 = [ (rDefaultOperationSize `evalsTo` OpSize16, Terminal OpSize32)
            , (rDefaultOperationSize `evalsTo` OpSize32, Terminal OpSize16)
            ]

-- | Overridden address size (OAS)
rOverriddenAddressSize :: X86Rule AddressSize
rOverriddenAddressSize = OrderedNonTerminal
      [ (pPrefix Prefix67, NonTerminal p67)
      , (CBool True      , rDefaultAddressSize)
      ]
   where
      p67 = [ (rDefaultAddressSize `evalsTo` AddrSize16, Terminal AddrSize32)
            , (rDefaultAddressSize `evalsTo` AddrSize32, Terminal AddrSize16)
            , (rDefaultAddressSize `evalsTo` AddrSize64, Terminal AddrSize32)
            ]

-- | Overriden operation size for 64-bit
--
-- Support W prefix and default operation size set to 64-bit in 64-bit mode.
rOverriddenOperationSize64 :: X86Rule OperandSize
rOverriddenOperationSize64 = OrderedNonTerminal
      [ (pMode64bit, OrderedNonTerminal
            [ (Or [ Predicate (InsnPred Default64OpSize)
                  , pPrefix PrefixW
                  ]
              , Terminal OpSize64
              )
            , (CBool True, rOverriddenOperationSize)
            ]
        )
      , (CBool True, rOverriddenOperationSize)
      ]

-- | Operand size predicate
pOpSize64 :: a -> a -> a -> a -> X86Rule a
pOpSize64 a b c d = OrderedNonTerminal
   [ (pForce8bit                         , Terminal a)
   , (pOverriddenOperationSize64 OpSize16, Terminal b)
   , (pOverriddenOperationSize64 OpSize32, Terminal c)
   , (pOverriddenOperationSize64 OpSize64, Terminal d)
   ]
