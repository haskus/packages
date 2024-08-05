#import "../helpers.typ" as haskus

== System Programming Interface <system-api>

=== How to manage devices?

Device management is the entry-point of system programming. Programs have to
know which devices are available to communicate with the user (graphic cards,
input devices, etc.) or with other machines (network cards, etc.).

In this guide, we present the basic concepts of device management and we show
examples with simple virtual devices provided by Linux.

==== How to enumerate available devices?

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

==== How to deal with hot-pluggable devices? <devices-hotplug>

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

==== How to make use of a device?

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

=== How to use the logging system?

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

=== How to display graphics?

==== Understanding the pipeline <graphics-pipeline>

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
  image("images/graphics_linux_model.svg"),
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

==== Listing displays <graphics-list-displays>

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


==== Generic buffers and frames <graphics-generic-buffers>

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


==== Configuring the pipeline <graphics-pipeline-config>

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
  image("images/graphics_first_pipeline.png"),
  caption: [
    Pattern displayed by the example code
  ]
)

==== Double-bufffering and frame-switching

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


