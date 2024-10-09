{-# LANGUAGE OverloadedLists #-}

module Haskus.Tests.Arch.X86_64.Asm
  ( testAsm
  )
where

import Haskus.Arch.X86_64.ISA.Encoder
import Haskus.Arch.X86_64.ISA.Encoding.Enc
import Haskus.Arch.X86_64.ISA.Encoding.Reg
import Haskus.Arch.X86_64.ISA.Encoding.Operand
import Haskus.Arch.X86_64.ISA.Encoding.Operation
import Haskus.Arch.X86_64.ISA.Optimizer
import Haskus.Arch.X86_64.ISA.Context

import Test.Tasty
import Test.Tasty.HUnit

import GHC.Exts
import GHC.Word
import Numeric (showHex)

testAsm :: TestTree
testAsm = testGroup "Assembler"
  [ testEncoding defaultContext64 DAA [] Nothing
  , testEncoding defaultContext16 DAA [] $ Just "27"
  , testEncoding defaultContext64 MOV [OpReg R_RAX, I8  0x17] Nothing
  , testEncoding defaultContext64 MOV [OpReg R_RAX, I32 0x17] $ Just "48c7c017000000"
  , testOptEncoding defaultContext64 MOV [OpReg R_RAX, I32 0x17] $ Just "b817000000"
  ]
  
-- | Encode with optimized assembly
testOptEncoding :: Context -> Operation -> Operands -> Maybe String -> TestTree
testOptEncoding ctx op ops result =
  case optimizeInsn defaultOptimOpts ctx op ops of
    Just (op',ops') -> testEncoding ctx op' ops' result
    Nothing         -> testEncoding ctx op  ops  result

testEncoding :: Context -> Operation -> Operands -> Maybe String -> TestTree
testEncoding ctx op ops result = testCase (show (op,ops)) $
  let str = case encodeInsn ctx op ops of
              Nothing  -> Nothing
              Just enc -> case encCheck enc of
                errs | not (null errs) -> error $ "Generated invalid encoding: " ++ show errs
                _ -> Just $ arrToString (encodeToArray# enc)
  in str @?= result

arrToString :: ByteArray# -> String
arrToString ba = go (sizeofByteArray# ba -# 1#) []
  where
    go 0# xs = showHex (W8# (indexWord8Array# ba 0#)) xs
    go i  xs = go (i -# 1#) (show_hex (W8# (indexWord8Array# ba i)) xs)

    show_hex x xs
      | x < 16   = '0' : showHex x xs
      |otherwise = showHex x xs


