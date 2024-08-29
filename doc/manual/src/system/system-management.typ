#import "../helpers.typ" as haskus

== System Management

=== How to build a system <build-system>

This section explains how to build a system: a Linux kernel and an init program.

*Important*: the recommended approach is to use the `haskus-system-build` tool
(@haskus-system-build) which does the following steps automatically. This
section is useful to understand what the tool is doing under the hood.

==== Building an `init` program <build-init-program>

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
  ../haskus-base
  ../haskus-utils-compat
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

==== Building the Linux kernel <build-linux-kernel>

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

==== Building the `initramfs` archive <build-initramfs>

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

=== How to test a system with QEMU <test-system>

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

=== How to distribute a system <distribute-system>

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


==== Downloading Syslinux

You first need to download and unpack the Syslinux boot-loader:

```bash
> wget http://www.kernel.org/pub/linux/utils/boot/syslinux/syslinux-6.03.tar.xz
> tar xf syslinux-6.03.tar.xz
```

==== Creating the disk directory

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


==== Creating a bootable device

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


==== Creating a bootable ISO

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


=== The haskus-system-build tool <haskus-system-build>

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

==== Command-line interface

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

==== `system.yaml` syntax <system-yml>


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


