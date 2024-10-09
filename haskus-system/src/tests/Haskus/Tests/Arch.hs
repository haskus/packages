module Haskus.Tests.Arch where

import Test.Tasty

import Haskus.Tests.Arch.Linux
import Haskus.Tests.Arch.X86_64

testsArch :: TestTree
testsArch = testGroup "Arch"
   [ testsLinux
   , testsX86_64
   ]
