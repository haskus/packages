= Introduction

The Haskus Computer Manual is a book in several volumes about computer
programming.

- The Haskus framework is written in Haskell. @volume-stdlib describes the
  general purpose modules provided by the `haskus-base` library that are used by
  other domain specific codes described in the other volumes.

  Note that `haskus-base` strives to have as few dependencies on other libraries
  as possible so that most of the code is self contained and easy to update/fix.
  It is an opiniated library with its own conventions (e.g. basic types such as
  `Word8` have been renamed into `U8`).

- @volume-arch documents computer architectures supported by the Haskus
  framework. E.g. architectures for which we provide an assembler. For now, only
  the x86 architecture is supported and documented but we expect to cover
  AArch64 and other architectures in the future.

- @volume-system is about system programming. It describes some Linux user space
  APIs and the abstraction we built on top of them in the `haskus-system`
  package. This package can be used to build system libraries and programs. We
  even provide a program named `haskus-system-build` to automatically build the
  Linux kernel and custom `init` programs and test them in QEmu.

- @volume-compilers is about engineering a compiler. This section is empty for
  now but it should be written when compilers will be provided.

- @volume-graphics is about computer graphics.
