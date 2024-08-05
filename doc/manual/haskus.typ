#set text(font: "Neo Sans Std")

#set heading(numbering: "1.1.1.1   ")
#show heading: it => {
  set text(
      weight: "medium"
    , rgb("#0860a8")
    , font: "Share Tech Mono"
  )
  v(8pt)
  it
}

#let trivia(x) = block(
    fill: rgb("#fae1df")
  , inset: 8pt
  , radius: 5pt
  , text(fill: rgb("#1c2826"), x)
)

#show raw.where(block: true): it => block(
    fill: rgb("#fae1df")
  , inset: 8pt
  , radius: 5pt
  , text(fill: rgb("#1c2826"), it)
)

#show raw.where(block: false): it => highlight(
    fill: rgb("#fae1df")
  , text(fill: rgb("#1c2826"), it)
)

// Palette:
// 0680A8 (blue)
// D64550 (red)
// FAE1DF (rose)
// 1C2826 (black)
// CFFFE5 (green)

#set document(
    title: "Haskus packages manual"
  , author: "Sylvain Henry"
  , date: auto
)

#set page(
    numbering: "1"
  , number-align: right
)

#outline()

= About haskus-system

*haskus-system* is a framework to develop systems based on the Linux kernel.
Its main selling points are:

- written in Haskell (systems are written in Haskell too)
- automatic building and testing (within QEMU) with the `haskus-system-build` tool
- (@haskus-system-build)
- easy to use and to modify:
  #link("https://github.com/haskus/packages/haskus-system/")[all the code in a
  single Git repository], documentation...
- reproducible builds: pinned versions of the Linux kernel (@system-yml) and of Haskell
  dependencies (using `Cabal`)

`haskus-system` is based *directly* and *exclusively* on the Linux kernel.
Hence, it doesn't rely on usual user-space kernel interfaces (e.g., `libdrm`,
`libinput`, `X11`, `wayland`, etc.) to communicate with the kernel.
We are free to use all the features provided by the kernel without being
constrained by the interfaces of other libraries. All the bugs are ours.

== About systems

Typical computer programs don't directly deal with hardware components
(processor, video chipsets, keyboard, mouse, network chipset, storage, etc.).
Instead they rely several abstractions provided by other software. The main one
being the #link("https://en.wikipedia.org/wiki/Kernel_(operating_system)")[kernel]
which directly handles the hardware with its device-specific drivers. It
provides a common framework that all the other programs have to use to access
the hardware.

#link("https://en.wikipedia.org/wiki/Linux_kernel")[Linux] is one of such kernels and
it is the one haskus-system uses. Typical computer programs usually don't use the interfaces
provided by the Linux kernel directly. They use abstractions built on top of
them and provided by system libraries which compose an
#link("https://en.wikipedia.org/wiki/Operating_system>")[operating system (OS)].

Many operating systems based on the Linux kernel use more or less the same
abstractions on top of it: these are the Unix-like
#link("https://en.wikipedia.org/wiki/Linux_distribution")[Linux distributions].
Examples of abstractions commonly used by these distributions: `libc`,
`libinput`, `Wayland`/`X11` server, `PulseAudio`, `dbus`, etc. As they differ
only in minor ways, they can execute the same applications (`Firefox`,
`LibreOffice`, etc.).

It is also possible to build non Unix-like operating systems on top of Linux.
These operating systems may provide totally different abstractions to the
applications. One example of such OS is
#link("https://en.wikipedia.org/wiki/Android_(operating_system)")[Android] which
mainly supports applications written in Java that use specific interfaces to
communicate with the hardware.

`haskus-system` is a framework that provides Haskell interfaces to the Linux
kernel (hence to the hardware). It also provides higher-level interfaces built
upon them. You can use these interfaces to build custom systems (Unix-like or
not). It is also up to you to decide if your system has the concept of
"application" or not: you may design domain specific systems which provide a
single domain specific application or game.

== Portability

Writing a new kernel from scratch is obviously a huge task.  Pragmatically
`haskus-system` relies on an existing kernel: Linux.

Note: the fact that `haskus-system` is based on the Linux kernel doesn't imply that
systems built with it have to provide a Unix-like interface to their
applications. This is similar to the approach followed by Google with Android:
the Linux kernel is used internally but applications have to be written in Java
and they have to use the Android interfaces.

`haskus-system` framework and the systems using it are written with the
Haskell language. We use GHC to compile Haskell codes, hence we rely on GHC's
runtime system. This runtime system works on a bare-bones Linux kernel and
manages memory (garbage collection), user-space threading,  asynchronous I/O,
etc. The runtime system has non-Haskell dependencies (libc, etc.) which are
statically linked with systems.

=== Supported architectures

The portability is ensured by the Linux kernel. In theory we could use our
approach on any architecture supported by the Linux kernel. In practice, we also
need to ensure that GHC supports the target architecture.

In addition, `haskus-system` requires a thin architecture-specific layer
because Linux interface is architecture specific. Differences between
architectures include: system call numbers, some structure fields (sizes and
orders), the instruction to call into a system call and the way to pass system
call parameters (calling convention).

The following architectures are currently supported by each level of the stack:

- haskus-system: x86-64
- GHC: x86, x86-64, PowerPC, and ARM
- Linux kernel: many architectures

=== Proprietary drivers

Some vendors do not provide open-source drivers nor documentation for their
hardware. Instead they provide pre-compiled libraries and/or kernel modules.  As
they presuppose the use of some system libraries and services (`OpenGL`,
`X11`, etc.), `haskus-system` doesn't support them.

== Performance

Using a high-level language such as Haskell is a trade-off between performance
and productivity. Just like using C language instead of plain assembly language
is. Moreover in both cases we expect the compilers to perform optimizations that
are not obvious or that would require complicated hard to maintain codes if they
were to be coded explicitly.

GHC is the Haskell compiler we use. It is a mature compiler still actively
developed. It performs a lot of optimizations. In particular, it performs
inter-modules optimizations so that well-organized modular code doesn't endure
performance costs.

Haskell codes are compiled into native code for the architecture (i.e., there is
no runtime interpretation of the code). In addition, it is possible to use LLVM
as a GHC backend to generate the native code.

The generated native codes are linked with a runtime system provided by GHC that
manages:

- Memory: garbage collection
- Threading: fast and cheap user-space threading
- Software transactional memory (STM): safe memory locking
- Asynchronous I/O: non-blocking I/O interacting with the threading system

Performance-wise, this is a crucial part of the stack we use. It has been
carefully optimized and it is tunable for specific needs. It is composed of
about 40k lines of C code.

As a last resort, it is still possible to call codes written in other languages
from Haskell through the Foreign Function Interface (FFI) or by adding a Primary
Operation (primop). ``haskus-system`` uses these mechanisms to interact with
the Linux kernel.

*Discussion*

It seems to us that this approach is a good trade-off. As comparison points,
most UNIX-like systems rely on unsafe interpreted shell scripts (init systems,
etc.); Google's Android (with Dalvik) used to perform runtime bytecode
interpretation and then just-in-time compilation, currently (with ART) it still
uses a garbage collector; Apple's platforms rely on a garbage collection variant
called "automatic reference counting" in Objective-C and in Swift languages
(while it might be more efficient, it requires much more care from the
programmers); JavaScript based applications and applets (unsafe language, VM,
etc.) tend to generalize even on desktop.

== Productivity

Writing system code in a high-level language such as Haskell should be much more
productive than writing it in a low-level language like C.

- High-level code is often more concise and most of the boilerplate code (e.g.,
  error management, logging, memory management) can be abstracted away. Fully
  working code examples stay shorts and understandable. Writing system code is
  much more fun as we can quickly get enjoyable results (less irrelevant details
  to manage).

- Many errors are caught during the compilation (type checking) which is
  especially useful with system programming because programs are harder to debug
  using standard methods (printf) and tools (gdb).

- Code is easier to refactor thanks to type-checking, hence more maintenable.

== Durability and Evolution

Our approach should be both durable and evolutive. Durable because we only use
mature technology: Linux and GHC developments both started in early 1990s and
are still very active. The only new layer in the stack is `haskus-system`
framework. All of these are open-source free software, ensuring long-term
access to the sources.

The approach is evolutive: Haskell language is evolving in a controlled way with
GHC's extensions (and a potential future Haskell standard revision); GHC as a
compiler and a runtime system is constantly improving and support for new
architectures could be added; Linux support for new hardware and new
architectures is constantly enhanced and specific developments could be done to
add features useful for `haskus-system` (or your own system on top of it).

`haskus-system` framework itself is highly evolutive. First it is new and
not tied to any standard. Moreover code refactoring in Haskell is much easier
than in low-level languages such as C (thanks to the strong typing), hence we
can easily enhance the framework interfaces as user code can easily be adapted.

== Single Code Base & Integration

In our opinion, a big advantage of our approach is to have an integrated
framework whose source is in a single code base. It makes it much easier to
evolve at a fast pace without having to maintain interface compatibility between
its internal components. Moreover, refactoring is usually safe and relatively
easy in Haskell, so we could later split it into several parts if needed.

#trivia[
As a comparison point, usual Linux distributions use several system services and
core libraries, most of them in their own repository and independently
developed: `libc`, `dbus`, `udev`, `libdrm`, `libinput`, `Mesa/X11/Wayland`,
`PulseAudio`, etc. It is worth noting that the issue has been identified and
that an effort has been recently made to reduce the fragmentation and to
centralize some of them into a more integrated and coherent framework:
`systemd`.
]

Having a single codebase written with a high-level language makes it easier to
find documentation, to understand how things work (especially the interaction
between the different components) and to make contributions.

== Standards

`haskus-system` can only be used on top of the Linux kernel. It doesn't
try to follow some standards (`UNIX`, `POSIX`, `System V`, etc.) to be
portable on other kernels. In our opinion, these standards have been roadblocks
to progress in system programming because system services and applications are
usually designed to follow the least common standards to ensure portability. For
instance, useful features specific to the Linux kernel may not be used because
some BSD kernels do not support them [See also the heated debates about
`systemd` requiring Linux specific features]. With our approach, we can use
every feature of the Linux kernel and develop new ones if needed.

It is often stated that programs should conform to the "UNIX philosophy":
each program should do only one thing and programs must be easily composable.
Despite this philosophy, `UNIX` systems often stand on feet of clay: programs are
composed with unsafe shell scripts and data exchanged between programs are
usually in weakly structured plain text format.

In our opinion, functional programming with strong typing is much more principled
than the "UNIX philosophy": functions are by nature easily composable and their
interfaces are well-described with types. In addition, we are not limited to
plain text format and the compiler ensures that we are composing functions in
appropriate ways.

#trivia[
As an example, compare this with `UNIX` standard commands such as `ls` which
include many result sorting flags while the `sort` command could be used
instead: the weakly structured output of the `ls` command makes it very
inconvenient to indicate on which field to sort by (*difficult to compose*).
Moreover, the output of the `ls` command must never change, otherwise many tools
relying on it may be broken (*not evolutive*). This is because most commands do
two things: compute a result and format it to be displayed, while they should
only do the first (according to the `UNIX` philosophy). We don't have this issue
because we use type-checked data types instead of plain text.
]

Even if `haskus-system` is in a single code base, its functions can be used in
other Haskell programs just by importing its modules. The compiler statically
checks that functions are appropriately called with valid parameters.

#trivia[
Compare this with the usual interface between two `UNIX` programs: parameters
from the first program have to be serialized as text and passed on the
command-line (with all the imaginable limitations on their sizes); then the
second program has to parse them as well as its standard input, to handle every
error case (missing parameter, invalid parameter, etc.), and to write the
result; finally the first program has to parse the outputs (both `stdout` and
`stderr`) of the second one and to react accordingly. For such a fundamental
concept, there is a lot of boilerplate code involved and many potential errors
lurking in it.
]

== Building and testing

`haskus-system` approach allows us to quickly have a working prototype that can be tested in
an emulated environment (e.g., with `QEMU`). Especially with the
`haskus-system-build` tool that automatizes all of
the building steps (see @haskus-system-build).

#trivia[
As a comparison point, building a minimal usual Linux distribution from scratch
is very cumbersome as we can read in the
"#link("http://www.linuxfromscratch.org/lfs")[Linux From Scratch] " book. A lot
of different packages have to be downloaded from various places, patched,
configured, built and installed. Even if our approach is currently far from
being on par with a usual Linux distribution, we expect it to stay much more
simpler to build.
]




= System Management

== How to build a system <build-system>

This section explains how to build a system: a Linux kernel and an init program.

*Important*: the recommended approach is to use the `haskus-system-build` tool
(@haskus-system-build) which does the following steps automatically. This
section is useful to understand what the tool is doing under the hood.

=== Building an `init` program <build-init-program>

Suppose we want to build the the following `HelloWorld` program
#footnote[System API is described in @system-api.]:

```haskell
import Haskus.System

main :: IO ()
main = runSys' <| do
   term <- defaultTerminal
   writeStrLn term "Hello World!"
   waitForKey term
   powerOff
```

We want to build a statically linked executable to avoid the need for
distributing a system loader and some libraries (`.so`). We accomplish this with
the following `executable` section in a `.cabal` file:

```cabal
executable HelloWorld
   main-is: HelloWorld.hs
   build-depends:
      base,
      haskus-system
   default-language: Haskell2010
   ghc-options:      -Wall -static -threaded
   cc-options:       -static
   ld-options:       -static -pthread
   #extra-lib-dirs: /path/to/static/libs
```

If static versions of the `libgmp`, `libffi` and `glibc` libraries (used
by GHC's runtime system) are not available on your system, you have to compile
them and to indicate to the linker where to find them: uncomment the last line
in the previous extract of the `.cabal` file (the `extra-lib-dirs` entry)
and modify it so that the given path points to a directory containing the static
libraries.

Then use a `cabal.project` file to declare dependencies on the haskus packages
you need. For example, in the `haskus-system-examples` directory, we use the
following `cabal.project` to depend on development versions of some packages:

```yaml
packages:
  .
  ../haskus-system
  ../haskus-utils-variant
  ../haskus-utils-types
  ../haskus-utils-data
  ../haskus-utils-compat
  ../haskus-utils
  ../haskus-binary
  ../haskus-ui

program-options
  ghc-options: -fhide-source-paths
```

Then use `cabal build HelloWorld` to compile the program.

Then use `cabal list-bin HelloWord` to localize the generated binary file that we will
use as `init` program:

```bash
> cabal list-bin HelloWorld
/home/hsyl20/projects/haskus/packages/haskus-system-examples/dist-newstyle/build/x86_64-linux/ghc-9.6.6/haskus-system-examples-0.1.0.0/x/HelloWorld/build/HelloWorld/HelloWorld
```

=== Building the Linux kernel <build-linux-kernel>

The Linux kernel is required to execute systems using `haskus-system`. Leaving
aside modules and firmwares, a compiled Linux kernel is a single binary file.

To build Linux, you first need to download it from https://kernel.org and to unpack
it:

```bash
> wget https://www.kernel.org/pub/linux/kernel/v4.x/linux-4.9.8.tar.xz
> tar xf linux-4.9.8.tar.xz
```

Then you need to configure it. We recommend at least the following:

```bash
> cd linux-4.9.8

# default configuration for the X86-64 target
> make x86_64_defconfig

# enable some DRM (graphics) drivers
> ./scripts/config -e CONFIG_DRM_BOCHS
> ./scripts/config -e CONFIG_DRM_RADEON
> ./scripts/config -e CONFIG_DRM_NOUVEAU
> ./scripts/config -e CONFIG_DRM_VIRTIO_GPU

# fixup configuration (use default values)
> make olddefconfig
```

If you know what you are doing, you can configure it further with:

```bash
> make xconfig
```

Finally, build the kernel with:

```bash
> make -j8
```

Copy the resulting kernel binary that you can use with QEMU for instance:

```bash
> cp arch/x86/boot/bzImage linux-4.9.8.bin
```

You can also copy built modules and firmwares with:

```bash
> make modules_install INSTALL_MOD_PATH=/path/where/to/copy/modules
> make firmware_install INSTALL_FW_PATH=/path/where/to/copy/firmwares
```

=== Building the `initramfs` archive <build-initramfs>

After its initialization, the Linux kernel creates a virtual file system in
memory (`initramfs`) and runs the `/init` program from it (or another program
specified by a kernel flag). We need to prepare an archive file containing the
files we want loaded in this virtual file system.

To do that, put the files you want (at least the `init` program built in
@build-init-program) into a directory `/path/to/my/system`.
Then
execute:

```bash
(cd /path/to/my/system ; find . | cpio -o -H newc | gzip) > myimage.img
```

You need to have the `cpio` and `gzip` programs installed. It builds a
ramdisk file named `myimage.img` in the current directory.

Note that `haskus-system-build` should soon use its own implementation of
`cpio`. See https://github.com/haskus/packages/issues/59

== How to test a system with QEMU <test-system>

You can test a system with QEMU. You only need:
- a linux kernel binary (see @build-linux-kernel): e.g. `linux.bin`
- an initramfs archive (see @build-initramfs), e.g. `myimage.img`

If the filepath of the `init` program to run in the `initramfs` is `/my/system`,
then you can test it with QEMU as follows:

```bash
qemu-system-x86_64
   -kernel linux.bin
   -initrd myimage.img
   -append "rdinit=/my/system"
```

We recommend the following options for `QEMU`:

```bash
# make QEMU faster by using KVM (if supported by your host system)
-enable-kvm

# use newer simulated hardware
-machine q35

# Increase the amount of memory available for the guest
-m 8G

# redirect the guest Linux console on the host terminal
-serial stdio
-append "console=ttyS0"

# make the guest Linux output more quiet
-append "quiet"
```

== How to distribute a system <distribute-system>

To distribute your systems, create a temporary directory (say `/my/disk`)
containing:

- your system (in an initramfs archive, cf @build-initramfs)
- the Linux kernel binary (cf @build-linux-kernel)
- the boot-loader files (including its configuration)

A boot-loader is needed as it loads Linux and the initramfs archive containing
your system. We use Syslinux boot-loader but you can use others such as GRUB.
Note that you don't need a boot-loader when you test your system with QEMU
because QEMU acts as a boot-loader itself.

To distribute your systems, you can install the boot-loader on a device (e.g.,
USB stick) and copy the files in the `/my/disk` directory on it. Or you can
also create a `.iso` image to burn on a CD-ROM (or to distribute online).


=== Downloading Syslinux

You first need to download and unpack the Syslinux boot-loader:

```bash
> wget http://www.kernel.org/pub/linux/utils/boot/syslinux/syslinux-6.03.tar.xz
> tar xf syslinux-6.03.tar.xz
```

=== Creating the disk directory

You need to execute the following steps to create your disk directory:

Create some directories:

```bash
> mkdir -p /my/disk/boot/syslinux
```

Copy Syslinux:

```bash
> find syslinux-6.03/bios *.c32 -exec cp {} /my/disk/boot/syslinux ;
> cp syslinux-6.03/bios/core/isolinux.bin /my/disk/boot/syslinux/
```

Copy the Linux kernel:

```bash
> cp linux-4.9.8.bin /my/disk/boot/
```

Copy the system ramdisk:

```bash
> cp myimage.img /my/disk/boot/
```

Finally, we need to configure the boot-loader by creating a file
`/my/disk/boot/syslinux/syslinux.cfg` containing:

```ini
DEFAULT main
PROMPT 0
TIMEOUT 50
UI vesamenu.c32

LABEL main
MENU LABEL MyOS
LINUX  /boot/linux-4.9.8.bin
INITRD /boot/myimage.img
APPEND rdinit="/my/system"
```

Replace `/my/system` with the path of your system in the ``myimage.img``
ramdisk.


=== Creating a bootable device

To create a bootable device (e.g., bootable USB stick), you have to know its
device path (e.g., `/dev/XXX`) and the partition that will contain the boot
files (e.g., `/dev/XXX_N`).

You can use `fdisk` and `mkfs.ext3` to create an appropriate partition.

You have to install Syslinux MBR:

```bash
> sudo dd bs=440 if=syslinux-6.03/bios/mbr/mbr.bin of=/dev/XXX
```

Then you have to copy the contents of the disk directory on the partition and
configure it to be bootable:

```bash
> sudo mount /dev/XXX_N /mnt/SOMEWHERE
> sudo cp -rf /my/disk/* /mnt/SOMEWHERE
> sudo syslinux-6.03/bios/extlinux/extlinux --install /mnt/SOMEWHERE/boot/syslinux
> sudo umount /mnt/SOMEWHERE
```

Now your device should be bootable with your system!


=== Creating a bootable ISO

To create a bootable CD-ROM, you first need to create a `.iso` disk image with
the `xorriso` utility:

```bash
xorriso -as mkisofs
   -R -J                            # use Rock-Ridge/Joliet extensions
   -o mydisk.iso                    # output ISO file
   -c boot/syslinux/boot.cat        # create boot catalog
   -b boot/syslinux/isolinux.bin    # bootable binary file
   -no-emul-boot                    # does not use legacy floppy emulation
   -boot-info-table                 # write additional Boot Info Table (required by SysLinux)
   -boot-load-size 4
   -isohybrid-mbr syslinux-6.03/bios/mbr/isohdpfx_c.bin  # hybrid ISO
   /my/disk
```

It should create a `mydisk.iso` file that you can burn on a CD or distribute
online.


== The haskus-system-build tool <haskus-system-build>

The `haskus-system-build` tool helps automating the steps required to build
(@build-system), to test (@test-system), and to distribute systems
(@distribute-system).

It requires the following externals programs (or commands):
- git
- tar, lzip, gzip, cpio
- make, gcc, binutils...
- cabal-install
- dd, (u)mount, cp
- qemu
- xorriso

=== Command-line interface

This is the reference for the `haskus-system-build` program command-line
interface.

Commands:

- `init`: create a new project from a template

   - `--template` or `-t` (optional): template name

- `build`: build the project and its dependencies (Linux)

- `test`: launch the project into QEMU

   - `--init` (optional): specify an init program. It overrides `ramdisk.init`
     in `system.yaml` (cf @system-yml)

- `make-disk`: create a directory containing the whole system

   - `--output` or `-o` (mandatory): output directory

- `make-iso`: create an ISO image of the system

- `test-iso`: test the ISO image with QEMU

- `make-device`: install the system on a device

   - `--device` or `-d` (mandatory): device path (e.g., `/dev/sdd`). For now,
     the first partition is used as a boot partition.

Note that the tool also builds `libgmp` as it is required to statically link
programs produced by GHC. Some distributions (e.g., Archlinux) only provide
`libgmp.so` and not `libgmp.a`.

=== `system.yaml` syntax <system-yml>


This is the reference for `system.yaml` files used by `haskus-system-build`
tool (@haskus-system-build).

*Linux kernel*

- `linux.source`: how to retrieve the Linux kernel

   - `tarball` (default)
   - `git` (not yet implemented)

- `linux.version`: which Linux version to use

   - requires `linux.version=tarball`

- `linux.options`:

   - `enable`: list of Linux configuration options to enable
   - `disable`: list of Linux configuration options to disable
   - `module`: list of Linux configuration options to build as module

- `linux.make-args`: string of arguments passed to `make`

*Ramdisk*

- `ramdisk.init`: name of the program to use as init program


*QEMU*

- `qemu.profile`: option profile to use

   - `default` (default): enable some recommended options
   - `vanilla`: only set required options (-kernel, etc.)

- `qemu.options`: string of additional arguments to pass to QEMU

- `qemu.kernel-args`: string of additional arguments to pass to the Kernel

*Example*

```yaml
linux:
  source: tarball
  version: 4.11.3
  options:
    enable:
      - CONFIG_DRM_BOCHS
      - CONFIG_DRM_VIRTIO
    disable:
      - CONFIG_DRM_RADEON
    module:
      - CONFIG_DRM_NOUVEAU
  make-args: "-j8"

ramdisk:
  init: my-system

qemu:
  profile: vanilla
  options: "-enable-kvm"
  kernel-args: "quiet"
```

= System Programming <system-api>
