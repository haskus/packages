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


= System Programming Interface <system-api>

== How to manage devices?

Device management is the entry-point of system programming. Programs have to
know which devices are available to communicate with the user (graphic cards,
input devices, etc.) or with other machines (network cards, etc.).

In this guide, we present the basic concepts of device management and we show
examples with simple virtual devices provided by Linux.

=== How to enumerate available devices?

`haskus-system` provides an easy to use interface to list devices as detected
by the Linux kernel.  To do that, use `defaultSystemInit` and
`systemDeviceManager` as in the following code:

```haskell
import Haskus.System

main :: IO ()
main = runSys do

  sys  <- defaultSystemInit
  term <- defaultTerminal
  let dm = systemDeviceManager sys

  inputDevs   <- listDevicesWithClass dm "input"
  graphicDevs <- listDevicesWithClass dm "drm"

  let
    showDev dev = writeStrLn term ("  - " <> show (fst dev))
    showDevs    = mapM_ showDev

  writeStrLn term "Input devices:"
  showDevs inputDevs

  writeStrLn term "Display devices:"
  showDevs graphicDevs

  powerOff
```

Linux associates a class to each device. The previous code shows how to
enumerate devices of two classes: `input` and `drm` (direct rendering manager,
i.e., display devices). If you execute it in `QEMU` you should obtain results
similar to:

```
Input devices:
  - "/virtual/input/mice"
  - "/LNXSYSTM:00/LNXPWRBN:00/input/input0/event0"
  - "/platform/i8042/serio0/input/input1/event1"
Display devices:
  - "/pci0000:00/0000:00:01.0/drm/card0"
  - "/pci0000:00/0000:00:01.0/drm/controlD64"
```

To be precise, we are not listing devices but event sources: a single device may
have multiple event sources; some event sources may be virtual (for instance the
`mice` input device is a virtual device that multiplexes all the mouse device
event sources and that is useful if you have more than one connected mouse
devices).

=== How to deal with hot-pluggable devices? <devices-hotplug>

We are now accustomed to (un)plug devices into computers while they are running
and to expect them to be immediately detected and usable (i.e., without
rebooting). For instance input devices (keyboards, mice, joysticks, etc.) or
mass storages. The operating system has to signal when a new device becomes
available or unavailable.

   Linux loads some drivers asynchronously to speed up the boot.  Hence devices
   handled by these drivers are detected after the boot as if they had just been
   plugged in.

`haskus-system` provides an interface to receive events when the state of the
device tree changes. The following code shows how to get and print these
events:

```haskell
import Haskus.System

main :: IO ()
main = runSys do

  term <- defaultTerminal
  sys  <- defaultSystemInit
  let dm = systemDeviceManager sys

  -- Display kernel events
  onEvent (dmEvents dm) \ev ->
    writeStrLn term (show ev)

  waitForKey term
  powerOff
```

If you execute this code in ``QEMU``, you should get something similar to:

```haskell
-- Formatting has been enhanced for readability
KernelEvent
  { kernelEventAction = ActionAdd
  , kernelEventDevPath = "/devices/platform/i8042/serio1/input/input3"
  , kernelEventSubSystem = "input"
  , kernelEventDetails = fromList
    [("EV","7")
    ,("KEY","1f0000 0 0 00")
    ,("MODALIAS","input:b0011v0002p0006e0000-e0,...,8,amlsfw")
    ,("NAME","\"ImExPS/2Generic ExplorerMouse\"")
    ,("PHYS","\"isa0060/serio1/input0\"")
    ,("PRODUCT","11/2/6/0")
    ,("PROP","1")
    ,("REL","143")
    ,("SEQNUM","850")]}
KernelEvent
  { kernelEventAction = ActionAdd
  , kernelEventDevPath = "/devices/platform/i8042/serio1/input/input3/mouse0"
  , kernelEventSubSystem = "input"
  , kernelEventDetails = fromList
    [("DEVNAME","input/mouse0")
    ,("MAJOR","13")
    ,("MINOR","32")
    ,("SEQNUM","851")]}
KernelEvent
  { kernelEventAction = ActionAdd
  , kernelEventDevPath = "/devices/platform/i8042/serio1/input/input3/event2"
  , kernelEventSubSystem = "input"
  , kernelEventDetails = fromList
    [("DEVNAME","input/event2")
    ,("MAJOR","13")
    ,("MINOR","66")
    ,("SEQNUM","852")]}
KernelEvent
  { kernelEventAction = ActionChange
  , kernelEventDevPath = "/devices/platform/regulatory.0"
  , kernelEventSubSystem = "platform"
  , kernelEventDetails = fromList
    [("COUNTRY","00")
    ,("MODALIAS","platform:regulatory")
    ,("SEQNUM","853")]}
```

The three first events are due to Linux lazily loading the driver for the mouse.
The last event is Linux asking the user-space to load the wireless regulatory
information.

=== How to make use of a device?

To use a device, we need to get a handle (i.e., a reference) on it that we will
pass to every function applicable to it. See the code here:
https://github.com/haskus/packages/haskus-system-examples/src/device-open/Main.hs

This code reads a 64-bit word from the `urandom` device that returns random data
and another from the `zero` device that returns bytes set to 0. Finally, we
write a string into the `null` device that discards what is written into it.
These three devices are virtual and are always available with Linux's default
configuration.

*Device Specific Interfaces*

In the previous code example we have used read and write methods as if the
device handle had been a normal file handle. Indeed Linux device drivers define
the operational semantics they want to give to each system call applicable to a
file handle: `read`, `write`, `fseek`, `mmap`, `close`, etc. Some
system calls may be invalid with some device handles (e.g., `write` with the
`urandom` driver).

This gives a weak unified interface to device drivers: the system calls are the
same but the operational semantics depends on the driver. Moreover there are a
lot of corner cases, such as system call parameters or flags only valid for some
drivers. Finally, as there aren't enough "generic" system calls to cover the
whole spectrum of device features, the `ioctl` system call is used to send
device specific commands to drivers. In practice you really have to know which
device driver you're working with to ensure that you use appropriate system
calls.

To catch up as many errors at compile time as possible, in `haskus-system` we
provide device specific interfaces that hide all this complexity. If you use
them, you minimise the risk of accidentally using an invalid system call. Some
of these interfaces are presented in the next chapters. Nevertheless you will
have to use the low-level interface presented in this chapter if you want to
write your own high-level interface to a device class not supported by
`haskus-system` or if you want to extend an existing one.

== How to use the logging system?

Many high-level interfaces of the `haskus-system` use the `Sys` monad. It is
basically a wrapper for the `IO` monad that adds a logging mechanism.

The following code prints the system log that is implicitly maintained in the
`Sys` monad on the kernel console.

```haskell
import Haskus.System

main :: IO ()
main = runSys do
  term <- defaultTerminal
  writeStrLn term "Hello World!"
  waitForKey term
  sysLogPrint -- print system log
  powerOff
```

Hence, the output of this program is something like:

```
Hello World!

---- Log root
--*- FORK: Terminal input handler
  |---- Read bytes from Handle 0 (succeeded with 1)
  |---- readBytes /= 0 (success)
--*- FORK: Terminal output handler

[    1.818814] reboot: Power down
```

You can see that the log is hierarchical and that it supports thread forks:
`defaultTerminal` forks 2 threads to handle asynchronous terminal
input/output; the input thread indicates in the log that it has read 1 byte from
the terminal input (when I have pressed the `enter` key).

Note that the log entries produced by the framework functions may change in the
future, hence the contents of the log may change and you may not get exactly the
same output if you try to execute this code.

== How to display graphics?

=== Understanding the pipeline <graphics-pipeline>

In this chapter, the idea is to understand the pipeline that describes where the
video displays get their pixel colors from. Configuring the pipeline is
explained in @graphics-pipeline-config.

This pipeline directly derives from the Linux kernel API named *kernel mode
setting* (KMS) (part of the *direct rendering manager* (DRM) interface) with
some naming differences though.

The left side of the following picture describes the relations between pipeline
entities (or objects) and the right side shows a more visual representation of
how an image is formed on a video display surface using the same entities.

#figure(
  image("images/system/graphics_linux_model.svg"),
  caption: [
    Linux Graphics pipeline model
  ]
)

To use a video display, our task is to build such valid pipeline. There are so
many possible hardware configurations (different graphics chipsets, different
video displays, etc.) that the KMS interface is very generic. It lets us build a
pipeline quite liberally and then we can test it before enabling it for real if
it is valid.

Controller and Plane entities of the graphics pipeline are invariant for each
graphics chipset. However Connectors are *not* invariant because some
technologies (such as
#link("https://en.wikipedia.org/wiki/DisplayPort#Multi-Stream_Transport_(MST)")[DisplayPort
Multi-Stream Transport])
allow the use of connectors hubs which dynamically add additional Connector
entities. Frames are managed by software hence they are not invariant either.

*Listing entities*

As our first code example in this tutorial, we will list all the entities of all
the graphic cards we find on the system.  The whole source code can be found
#link("https://github.com/haskus/packages/blob/master/haskus-system-examples/src/tutorial/TutEntitiesIDs.hs")[here].

We load all the graphic cards with:

```haskell
sys   <- defaultSystemInit
cards <- loadGraphicCards (systemDeviceManager sys)
```

Then we get entity identifiers with:

```haskell
mids <- runE (getEntitiesIDs card)
```

The rest of the code deals with errors and printing the results on the terminal.

The best way to test this code is to use `haskus-system-build` tool (cf
@haskus-system-build).

```bash
> git clone https://github.com/haskus/packages.git
> cd packages/haskus-system-examples
> haskus-system-build test --init TutEntitiesIDs

===================================================
       Haskus system - build config
---------------------------------------------------
GHC version:      9.6.6
Linux version:    6.9.9
Syslinux version: 6.03
Init program:     TutEntitiesIDs
===================================================
==> Building with Cabal...
[...]
==> Building ramdisk...
24986 blocs
==> Launching QEMU...
Card 0
 - Connector 31
 - Controller 35
 - Plane 33
[    1.026338] reboot: Power down
```

The tool should download, build and install the necessary dependencies and
execute the resulting system into `QEMU`. You can see that it reports one
Connector, one Controller and one Plane for the QEMU simulated graphics chipset
(the numbers are their unique identifiers).

=== Listing displays <graphics-list-displays>

To list the available video displays that are connected to the computer, we just
have to query the Connector entities and check if there is a video display
connected to them.

The whole source code for this chapter can be found
#link("https://github.com/haskus/haskus-system/blob/master/haskus-system-examples/src/tutorial/TutDisplays.hs")[here].
It detects the available video displays and reports information about them.

- We retrieve information about all the entities for each card with `getEntities card`

- Connectors (retrieved with `entitiesConnectors`) have `connectorType` and
  `connectorByTypeIndex` fields which can be used to query the kind of connector (HDMI,
  VGA, DVI, etc.) and the connector index for the given kind of connector.

- Connectors also have a `connectorState` field which can be used to detect
  connected display:

```haskell
case connectorState conn of
   Disconnected      -> -- no connected display
   ConnectionUnknown -> -- we can't know
   Connected display -> -- we have a connected display!
```

- We get the supported modes of the display with `displayModes` field of
  `display`, physical size in millimeters with `displayPhysicalWidth/Height`, the
  sub-pixel layout with `displaySubPixel` (can be used to perform
  #link("https://en.wikipedia.org/wiki/Subpixel_rendering")[sub-pixel rendering])
  and other properties with `displayProperties`.

Hint: we could have used `forEachConnectedDisplay` function to do all of this
listing and filtering more simply.

Example of run into QEMU with Linux 5.1.15:

```
> git clone https://github.com/haskus/packages.git
> cd packages/haskus-system-examples
> haskus-system-build test --init TutDisplays

Probing Connector 33: Virtual-1
Physical size: 0mm X 0 mm
Sub-pixel layout: SubPixelUnknown
Modes
1024x768 60MHz -HSync -VSync
        h: width 1024 start 1048 end 1184 total 1344 skew    0
        v: width  768 start  771 end  777 total  806 scan    0
1920x1080 60MHz -HSync -VSync
        h: width 1920 start 2008 end 2052 total 2200 skew    0
        v: width 1080 start 1084 end 1089 total 1125 scan    0
1600x1200 60MHz +HSync +VSync
        h: width 1600 start 1664 end 1856 total 2160 skew    0
        v: width 1200 start 1201 end 1204 total 1250 scan    0
[...]
Properties
    var DPMS = On :: Enum [On,Standby,Suspend,Off]
    var link-status = Good :: Enum [Good,Bad]
    val non-desktop = False :: Bool
    var CRTC_ID = 0 :: Object
```

*Detecting Plugging/Unplugging*

To detect when a video display is connected or disconnected, we could
periodically list the Connectors and check their ``connectorState`` property as
we have done above.

However a better method is to use a mechanism explained in @devices-hotplug:
when the state of a Connector changes, the kernel sends to the user-space an
event similar to the following one:

```haskell
KernelEvent
  { kernelEventAction = ActionChange
  , kernelEventDevPath = "/devices/.../drm/card0"
  , kernelEventSubSystem = "drm"
  , kernelEventDetails = fromList
    [("DEVNAME","drm/card0")
    ,("MAJOR","226")
    ,("MINOR","0")
    ,("HOTPLUG","1")
    ,("SEQNUM","1259")]}
```

When our system receives this event, we know it has to check the state of the
Connectors.

Also remember that Connector entities can appear and disappear at runtime.
That's because some technologies (such as
#link("https://en.wikipedia.org/wiki/DisplayPort#Multi-Stream_Transport_(MST)")[DisplayPort
Multi-Stream Transport]) allow the use of connectors hubs which increases the
number of video displays that can be connected at the same time.


=== Generic buffers and frames <graphics-generic-buffers>

The top-most elements of the graphics pipeline (@graphics-pipeline) are
Buffers. A Buffer is a memory region accessible by the graphics chipset. In this
case they are used to store pixel color components (e.g. 32-bit RGBA values).

There are two general kinds of buffers:

- driver specific buffers: they can be used to fully exploit the features of the
  device but they use different APIs depending on the device driver.

- generic buffers: these buffers use the same API for all devices supporting
  them but they can't be used to exploit the most advanced features of the
  device.

In this chapter we present the *generic buffers* as they are simple to use and
avalable for many graphics chipsets. The source code for this chapter can be
found #link("https://github.com/haskus/haskus-system/blob/master/haskus-system-examples/src/tutorial/TutGenericFrame.hs")[here].

*Allocating Generic Buffers*

To allocate a generic buffer, we can use `createGenericBuffer` function. It
takes a width and an height in pixels and the number of bits per pixels (must be
a multiple of 8). 

However we often want to create a Frame and the buffers according to the Frame
format. Hence in the code example we use ``createGenericFrame`` instead which
does all of this. Then it displays information about the allocated Frame and
Buffers.

Example of run into QEMU with Linux 5.1.15:

```bash
> git clone https://github.com/haskus/packages.git
> cd packages/haskus-system-examples
> haskus-system-build test --init TutCreateGenericFrame

Frame 35
  Width:  1024 pixels
  Height: 768 pixels
  Pixel format: XRGB8888 (LittleEndian)
  Flags: []
  FrameBuffer 0:
    - Buffer handle: 1
    - Pitch: 4096
    - Offset: 0
    - Modifiers: 0
    - Buffer specific details:
       - Type: generic buffer
       - Width:  1024 pixels
       - Height: 768 pixels
       - Bits per pixels: 32
       - Flags: 0
       - Handle: 1
       - Pitch: 4096 bytes
       - Size: 3145728 bytes
       - Address: 0x00007efd406f5000

[    0.984017] reboot: Power down
```

You can see that a `GenericBuffer` object contains the effective size of the
buffer (in bytes), the pitch (effective width of a line in bytes), an address
(explained in the next section), etc.

Hint: if we want to create a generic Frame for a full screen mode, we can pass a
`Mode` to `createGenericFullScreenFrame`. Frame and Buffers dimensions (in
pixels) are then obtained from the Mode.

*Writing into Generic Buffers*

Generic buffers have a very useful property: they can be mapped into the process
memory. `haskus-system` automatically maps them when they are created so you
don't have to worry about doing it.

The mapped region address is stored in a `ForeignPtr` which you can use with:
`withGenericBufferPtr` (or `withGenericFrameBufferPtr` wrapper). For example:

```haskell
-- fill the frame with a color
withGenericFrameBufferPtr fb \ptr ->
  forEachFramePixel frame \x y -> do
    let off = frameBufferPixelOffset fb 4 x y -- 4 is pixel component size in bytes
    pokeByteOff (castPtr ptr) (fromIntegral off) (color :: Word32)
```

However with generic buffers it is even easier: there is the
`forEachGenericFramePixel` wrapper that does these tricky pointer computations
for us. The code example uses it as follows:

```haskell
-- fill the generic frame with a color
-- (0 is the FrameBuffer index)
forEachGenericFramePixel frame 0 \_x _y ptr ->
   poke ptr (0x316594 :: Word32) -- write a XRGB8888 color
```


=== Configuring the pipeline <graphics-pipeline-config>

At this stage we already know enough things to
#link(<graphics-list-displays>)[detect a connected video display] and to
#link(<graphics-generic-buffers>)[allocate generic buffers and frames]. We just
need to learn how to connect entities to build
#link(<graphics-pipeline>)[graphics pipeline] and finally display something on
the screen.

The whole source code can be found
#link("https://github.com/haskus/haskus-system/blob/master/haskus-system-examples/src/tutorial/TutFirstPipeline.hs")[here].

We first need to find a primary plane and check the pixel formats it supports.
Then we need a Controller that can work with the plane (obtained with
`planePossibleControllers`). Finally we need to build the pipeline with code
similar to the following one:

```haskell
assertLogShowErrorE "Config" <| withModeBlob card mode \modeBlobID ->
  configureGraphics card Commit EnableVSync EnableFullModeset do
    setConnectorSource conn ctrlID -- connector  <-> controller
    setPlaneTarget plane ctrlID    -- controller <-> plane
    setPlaneSource plane frame     -- plane      <-> frame
    -- sizes and mode
    setPlaneSize plane (frameWidth frame) (frameHeight frame)
    setPlaneSourceSize plane (frameWidth frame) (frameHeight frame)
    setMode ctrlID modeBlobID
    -- enable the controller
    enableController ctrlID True
```

We allocate a Mode blob on the graphic card with `withModeBlob`. Then we use
`configureGraphics` to send a batch of commands to the chipset: the commands
will either all succeed or none of them will be applied (atomicity).

To test that a batch of commands is valid, you can pass `TestOnly` instead of
`Commit` and check for errors.

You can allow or disallow a full mode-setting with `EnableFullModeset` and
`DisableFullModeset`. A full mode-setting is costly so avoid it if you can
afford to run in a degraded mode for some time.

You can enable or disable vertical synchronization (VSYNC) (cf @graphics-vsync).
It is recommended to enable it to avoid tearing and because some drivers seem to
require it.

The example code should display the following pattern on the screen:

#figure(
  image("images/system/graphics_first_pipeline.png"),
  caption: [
    First Linux Graphics pipeline
  ]
)

=== Double-bufffering and frame-switching

In this section, we see that directly modyfing the frame that is displayed on
the screen leads to tearing/flickering and that double-buffering should be used
instead.

Starting from the code of the previous section (@graphics-pipeline-config) ,
suppose that we modify the contents of the frame continuously as we do 
#link("https://github.com/haskus/haskus-system/blob/master/haskus-system-examples/src/tutorial/TutSingleFrame.hs")[here]:

```haskell
let render frame col = do
     -- fill a frame with a color
     forEachGenericFramePixel frame 0 \_x _y ptr -> do
       poke ptr (col :: Word32)

    renderLoop col = do
      render frame col
      renderLoop (col + 10) -- change the color for each frame

sysFork "Render loop" (renderLoop 0)
```

If we try to execute this example, we see some flickering: sometimes the
displayed frame is not fully repaint and it has two different colors, that's why
we see some vertical line demarcating both colors.

This is a common issue that can be solved either by:

- only doing the rendering during the vblank period
- using two different frames and switching them during the vblank period

The first solution only requires a single buffer but your application has to
render each frame very fast during the vblank period and before the end of the
refresh cycle.

Using double-buffering is easier. #link("https://github.com/haskus/haskus-system/blob/master/haskus-system-examples/src/tutorial/TutFrameSwitch.hs")[Source code of the modified code example
using double-buffering].
The change consists in allocating two frames, rendering in one when the other is
displayed and switching the frames when the rendering is over:

```haskell
let switchFrame frame = assertLogShowErrorE "Switch frame" <| do
      configureGraphics card Commit EnableVSync DisableFullModeset do
        setPlaneSource plane frame

frame1 <- createGenericFullScreenFrame card mode pixelFormat 0
frame2 <- createGenericFullScreenFrame card mode pixelFormat 0

let renderLoop b col = do
      let frame = if b then frame1 else frame2
      render frame col
      switchFrame frame
      renderLoop (not b) (col + 10)

sysFork "Render loop" (renderLoop False 0)
```

When we execute this second example, the displayed frame is always fully
rendered and we don't get the flickering effect.

= System Internals <system-internals>

== Device management <device-management>

Internally `haskus-system` mounts a `sysfs` virtual file system through
which the Linux kernel exposes the hardware of the machine. In this file-system
each device is exposed as a sub-directory in the `/devices` directory and the
path to the device's directory uniquely identifies the device in the system.

Directory nesting represents the device hierarchy as the system sees it.
Regular files in device directories represent device properties that can be
read and sometimes written into from user-space.  Sometimes, when the tree
relationship between devices is not sufficient, relations between devices are
represented as symbolic links.

=== File descriptor vs Handle

Linux allows programs in user-space to have handles on kernel objects.
Suppose the kernel has an object `A` and a reference `R_A` on `A`.  Instead of
directly giving `R_A` to user-space processes, the kernel maintains a
per-process array of kernel object references: `D_pid` for the process with
the `pid` identifier.  To "give" `R_A` to this process, the kernel finds
an empty cell in `D_pid`, put `R_A` in it and gives the index of the cell to
the process.

For historical reasons, the cell index is called a file descriptor and `D_pid`
a file descriptor table even if in Linux they can be used for kernel objects
that are not files (e.g., clocks, memory). User-space processes can only refer
to kernel objects through theses indirect references. Note that the file
descriptor table is specific to each process: sharing a file descriptor with
another process does not allow to share the referred kernel object.

In `haskus-system` we use the term "handle" instead of "file descriptor" as we
find it less misleading.

=== Device special files and /dev

Ideally there would be a system call to get a handle on a device by providing
its unique identifier (similarly to the `getDeviceHandleByName` API provided by
`haskus-system`). Sadly it's not the case. We have to:

1. Get the unique device triple identifier from its name

   Linux has two ways to uniquely identify devices:

   - a path in `/devices` in the `sysfs` file-system
   - a triple: a major number, a minor number and a device type (`character` or
     `block`).

   `haskus-system` retrieves the triple by reading different files the the
   `sysfs` device directory.

2. Create and open a device special file

   With a device triple we can create a special file (using the `mknod` system
   call).

   `haskus-system` creates the device special file in a virtual file system
   (`tmpfs`), then opens it and finally deletes it.

Usual Linux distributions use a virtual file-system mounted in `/dev` and
create device special files in it. They let some applications directly access
device special files in `/dev` (e.g., X11). Access control is ensured by file
permissions (user, user groups, etc.). We don't want to do this in
`haskus-system`: we provide high-level APIs instead.


=== Netlink socket

Linux dynamically adds and removes files and directories in the `sysfs`
file-system, when devices are plugged or unplugged. To signal it to user-space,
it sends kernel events in a Netlink socket. The Netlink socket is also used to
pass some other messages, for instance when the kernel wants to ask something to
the user-space. `haskus-system` handles a Netlink socket, parses received
kernel events and delivers them through a STM broadcast channel.

In usual Linux distributions, a daemon called `udev` is responsible of
handling these kernel events. Rules can be written to react to specific events.
In particular, `udev` is responsible of creating device special file in the
`/dev` directory. The naming of theses special files is a big deal for these
distributions as applications use them directly afterwards and don't use the
other unique device identifiers (i.e., the device path in the `sysfs`
file-system).  In `haskus-system`, high-level APIs are provided to avoid
direct references to device special files.


=== Miscellaneous

In usual Linux distributions, `udev` (`man 7 udev`) is responsible of
handling devices. It reads `sysfs` and listens to kernel events to create and
remove device nodes in the `/dev` directory, following customizable rules.  It
can also execute custom commands (`crda`, etc.) to respond to kernel requests.


== Graphics

=== "Software" rendering (i.e. on the CPU)

For now in this manual we present a simple approach to display rendering:
basically the picture to display is generated by the CPU in host memory and then
transferred to the GPU memory (implicitly by using memory mapping). Most recent
graphic cards propose more efficient approaches: the picture to display is
generated and transformed directly by the graphic card. Instead of sending a
picture, the host sends commands or programs to be executed by the GPU.

Currently Linux doesn't propose a unified interface to advanced graphic card
capabilities from different vendors (these are usually handled by MESA in
user-space and accessed through the OpenGL interface). `haskus-system` doesn't
provide support for them yet.

=== Proprietary drivers

`haskus-system` uses the Kernel Mode Setting (KMS) and the Direct
Rendering Manager (DRM) interfaces. In usual Linux distributions, some graphic
card manufacturers provide closed-source proprietary drivers that do not support
theses interfaces: they use a kernel module and user-space libraries that
communicate together by using a private protocol. The user-space libraries
provide implementations of standard high-level interfaces such as OpenGL and can
be used by rendering managers such as X.org. `haskus-system` doesn't offer a
way to use these drivers.

=== Further reading

The two main acronyms for Linux's display model are KMS (standing for "kernel
mode-setting") and DRM (standing for "direct rendering manager").

As explained in the @device-management, device drivers can support
the `ioctl` system call to handle device specific commands from the
user-space. The display interface is almost entirely based on it. Additionally,
`mmap` is used to map graphic card memory in user-space and `read` is used
to read events (V-Blank and page-flip asynchronous completion).

In usual Linux distributions, the `libdrm` library provides an interface over
these system calls. You can learn about the low-level interface by reading the
`drm` manual (`man drm`, `man drm-kms`, etc.) or its source code at
https://cgit.freedesktop.org/mesa/drm/.

David Herrmann has written
#link("https://dvdhrm.wordpress.com/?s=drm-mode-setting")[a good tutorial]
explaining how to use the legacy low-level display interface in the form of C
source files with detailed comments. While some details of the interface have
changed since he wrote it (e.g., the way to flip frame buffers and the atomic
interface), it is still a valuable source of information.

The newer atomic interface is described in an article series on LWN called
"Atomic mode setting design overview" (August 2015) by Daniel Vetter:
- https://lwn.net/Articles/653071/
- https://lwn.net/Articles/653466/

Wayland is the new display system for usual
Linux based distributions. It can be a great source of inspiration and of
information:
- http://wayland.freedesktop.org

You can also read the Linux kernel code located in `drivers/gpu/drm` in the
kernel sources.

Multi-GPU is supported by Linux. In particular:

- Buffer sharing is supported with
#link("https://01.org/linuxgraphics/gfx-docs/drm/drm-memory-management.html\#drm-prime-support")[DRM
Prime]
- GPU switching is supported with #link("https://01.org/linuxgraphics/gfx-docs/drm/vga_switcheroo.html")[vga_switcheroo]

https://www.elinux.org/images/4/45/Atomic_kms_driver_pinchart.pdf

KMS/DRM history:

- https://libv.livejournal.com/13443.html
- https://ppaalanen.blogspot.com/2014/06/from-pre-history-to-beyond-global.html
- https://lwn.net/Articles/653071 and https://lwn.net/Articles/653466/


= Computer Graphics

== Human Vision


Video display technology is based on human vision in several ways:

*Color perception*

The human eye uses 3 kinds of sensors (in daylight) called
#link("https://en.wikipedia.org/wiki/Cone_cell")[cones] , each covering some
part of the #link("https://en.wikipedia.org/wiki/Visible_spectrum")[visible
spectrum]. Their behavior is quite complicated because their response ranges
overlap, the count of each sensor differs, the light perception is non-linear,
etc. For the human eye, different
#link("https://en.wikipedia.org/wiki/Spectral_power_distribution")[spectral
power distributions] can lead to the same perceived color
#link("https://en.wikipedia.org/wiki/Metamerism_(color)")[metamerism] .

Long story short, a video display doesn't have to be able to produce every
frequency of the visible spectrum to reproduce colors: it just has to produce 3
of them to trigger each kind of cone at a different level. Typically a video
display produces color by combining different amount of red, green and blue
lights which roughly correspond to each cone response peak.

Usually video display devices can only produce a subset of the visible color
space (or #link("https://en.wikipedia.org/wiki/Gamut")[gamut]).

*Spatial integration*

If several lights of different colors are emitted very closely to each other,
our eyes can't distinguish them and we don't perceive the gap between them.
Instead we only perceive another color which is composed of the emitted ones.

Screens display an array of lights called pixels (say 1920x1080 pixels) which
are very close one to the other so that we perceive a continuous picture without
distinguishing the gaps between each pixel individually (at normal viewing
distance of the video display).

Screens display pixel themselves as a combination of several (at least 3 but
sometimes more) distinct lights (red, green and blue) which are so close that we
perceive them as a single color.


*Temporal integration*

Similarly to the spatial integration, our eyes cannot distinguish lights that
change too fast over time. Instead we perceive an integration of the light over
time.

Screens use this to simulate continuous motion: if different images are
displayed very quickly, our eyes don't distinguish each picture individually.
Some video displays even emit a single line of pixels at a time or a single pixel
at a time, or alternate between even and odd lines for each frame.

Note that our peripheral vision is better at detecting motion. It explains why
virtual reality (VR) headsets have higher refresh rates.

*Summary*

Rendering an image on a video display surface consists in indicating the color
of each pixel. Note that pixels are samples of the image we would like to render
on the video display surface if it had an infinite resolution,
#link("http://alvyray.com/Memos/CG/Microsoft/6_pixel.pdf")[not "small squares"].

We have to render many different images per seconds to simulate moving images.

I recommend watching this video:
#link("https://www.youtube.com/watch?v=3BJU2drrtCM")[How a TV works in slow
motion]


== Video Display

A video display usually shows a rectangle of pixels. When connected to a
computer, it is managed by a graphics chipset which indicates to the video
display the color of each pixel of the current image to show several times per
second.

The algorithm used by the graphics chipset is roughly:

```
For each pixel in the current image
   Read its color from memory
   Convert the color into something intelligible for the video display
   Send the converted pixel color to the video display

Wait until it's time to send the next image (vertical blanking interval)
```

=== Refresh rates

The number of images (or frames) sent to the video display depends on its
refresh rate. Most video displays have a fixed (configurable) refresh rate. For
instance 60Hz (60 frames per second).

Newer video displays may also support variable refresh rates where the delay
between two frames may depend on the next frame being ready (or a timeout if it
isn't ready for too long).

=== Switching between frames <graphics-vsync>

If we modify the memory containing the pixel colors while the graphics chipset
is reading it, the video display may show incoherent pixels (e.g. pixels from
two different frames). This is called *tearing*. We usually want to avoid it.

The idea is to modify the pixel colors only while the graphics chipset is
waiting between two transfers. For historical reasons this period of time is
called the
#link("https://en.wikipedia.org/wiki/Vertical_blanking_interval")[vertical
blanking interval (VBI or VBLANK)].

If the software can't render a frame fast enough during the VBI, we usually want
the previous frame to be sent again. In order to do that, we use two buffers
(*double-buffering*): one buffer contains the current frame that is sent
repeatedly to the video display and the other contains the frame the software is
currently rendering. Once the next frame is ready we only have to switch the
buffer pointer (called *page-flipping*) during the VBI to inverse the roles of
the two buffers.

If the software renders frames too fast we can either block it until
page-flipping occurs or we can use *triple-buffering*: one buffer contains the
current frame as before, the second contains a pending frame (if any) which is a
frame ready to be displayed, and the third one contains the frame being rendered
as before. When the rendering in the third buffer is done, there is a switch
between the second and the third buffer (i.e. one of the rendered frame may not
be displayed at all if there was already a pending frame). During the VBI, if
there is a pending frame in the second buffer, there is a switch between the
first and the second buffers.

=== Plane composition

Instead of using a single source for the pixel colors, some graphics chipsets
allow the use of several pixel sources that are blended/composed together.

A *plane* describes a portion of the video display surface that uses a specific
source of pixel colors. Basic graphics chipsets only have a single primary plane
that occupies the whole video display surface; other graphics chipsets may have
several other planes with different properties (pixel formats, dimensions,
rotation, scaling, etc.).

This is particularly useful for what is called *hardware cursor*. A small cursor
plane is dedicated to display the mouse cursor so when the mouse moves we only
have to change the position of the cursor plane independently of the other
planes. It makes the cursor much more responsive because this operation is very
cheap.

Planes can also be used to render hardware decoded videos, overlays, etc.

*Summary*

The graphics chipset sends the pixel colors from the memory to the connected
video display several times per second (depending on the refresh rate).

The graphics chipset supports at least one primary plane but it can also support
additional planes (overlay, cursor, etc.) with additional properties (scaling,
rotation, different pixel format, etc.).

The software is responsible of producing pixel colors for each plane. To avoid
tearing, the switch from one frame to the other must be done during the vertical
blanking interval (VBI or VBLANK). Double- or triple-buffering can be used for
this purpose.
