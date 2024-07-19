[![GitHub CI](https://github.com/haskus/haskus-system/workflows/CI/badge.svg)](https://github.com/haskus/haskus-system/actions)

# Haskus system

Haskus system is a framework written in Haskell and designed for system
programming. Fundamentally it is an experiment into providing an integrated
interface leveraging Haskell features (type-safety, STM, etc.) for the whole
system: input, display, sound, network, etc.

Website: http://www.haskus.org/system

Documentation: https://docs.haskus.org/system/intro.html

# Building systems

The [haskus-system-build](https://github.com/haskus/haskus-system-build.git)
tool (in the package of the same name) is the preferred way to build systems.

You can install it from source with:

```bash
$ git clone https://github.com/haskus/packages.git
$ cd packages
$ cabal install haskus-system-build
```

It will install the program into ~/.cabal/bin. Be sure to add this path to your
$PATH environment variable.

Then try building and running an example:

```bash
$ cd haskus-system-examples
$ haskus-system-build test --init Clock
```

This should build everything required to execute qemu with the Clock example and
run qemu for you.

You may have to install missing programs (cpio, lzip, qemu, make, gcc, binutils,
gzip, etc.) for these commands to succeed. See the
[documentation](https://docs.haskus.org/system/building/automatic_building.html#building-and-testing).
