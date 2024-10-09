module Haskus.Tests.Arch.X86_64 where

import Test.Tasty

import Haskus.Tests.Arch.X86_64.Asm

testsX86_64 :: TestTree
testsX86_64 = testGroup "X86-64"
   [ testAsm
   ]
