== Graphics

=== Rendering

==== FrameBuffer Displaying

setControllerFB

==== Buffer Mapping

- Mapping buffers in user-space
- Raw drawing
- Example rectangle
- Dirty fb

==== Configuring The Display

- Mode settings
- automatic configuration for the connected devices in preferred modes, no cloning

===== Setting the display mode

The following code shows a code that tries every available mode of
a connector in sequence: each mode stays enabled for 4 seconds. To be allowed to
change the display mode, we need to create an adapted framebuffer (i.e., a
source of pixel data). You can safely ignore that for now, we will explain it in
details in Section~\ref{sec:framebuffer}.

// codes/src/DisplayModes.hs

- mode not supported by display device: garbage, black screen, error screen


==== Drawing On The Screen

- Generic framebuffer
- full drawing example

- Example DPMS (power-off)

==== Multiple-Buffering And V-Blank Synchronization

Computer screens usually have a fixed refresh rate. For instance in
Listing~\ref{lst:display_connectors_result} the first mode of the connected
display device has a (vertical) refresh rate of 60Hz. That is, 60 times per
second, the display device:

- copies the contents of the framebuffer on the screen line-by-line: *scan-out* period
- waits for some time until the next scan-out: *v-blank* period

===== Single Frame Buffer

Suppose we use a single frame buffer that is displayed on the screen. If we
modify its content during the scan-out period, some artifacts may appear on the
screen. For instance if we repeatedly clear the screen, then draw a filled
rectangle and finally draw a circle on top of it, the display may either show
the cleared screen, the rectangle alone or the rectangle with the circle. The
rectangle and the circle seem to flash: this is called a *flicker* effect.

- time to render a frame vs refresh period vs v-blank period

- Explanation scan-out (flikering?)
- Explanation multi-buffering
- Code framebuffer swap ("page flip")
- Explanation v-blank (tearing?)
- Code synchro v-blank (event v-blank)
- Note "async page flip" flag and "page flip complete" event
- Adaptive v-sync
- Dithering (frame rate control, TN panels 6-bits per RGB)

==== Advanced Topics

===== Atomic Configuration

Some drivers support an atomic configuration operation. Instead of setting a
property at a time, several properties are set at once in a transaction: if a
proprety fails to be set, the whole transaction is reverted. It ensures that the
display configuration is never left in a transition state between two
configurations.\footnote{At the time of writing, ``haskus-system`` doesn't
support this feature.}

===== Gamma Table

- theory, tests avec qemu non concluants

Gamma correction consists in altering the way colors are displayed on a monitor.
For each possible value of each color channel (red, green, blue),we can define a
gamma factor. Usually there are 256 possible values per channel, but the
additional \texttt{controllerGammaTableSize} field gives the actual number of
values. Each factor is an unsigned 16-bit word.

- Word16 or 8.8 fixed float?

The following code shows how to retrieve and show the gamma look-up
table of a controller with ``getControllerGamma``. Similarly you can set
another gamma table with ``setControllerGamma``.

// codes/src/DisplayGamma

===== Sub-Pixel Rendering

- Controller sub-pixel
- Used for fonts
- Vertical vs horizontal
- rotation property!

=== Multi-Buffering

==== Vertical synchronization

Computer screens usually have a fixed refresh rate. For instance if the 
display device has a (vertical) refresh rate of 60Hz, it means that 60 times per
second, the display device:

- copies the contents of the framebuffer on the screen: scan-out period
- waits for some time until the next scan-out: `vertical blanking interval`
  (v-blank interval)

If we modify the contents of the framebuffer that is displayed on the screen
during the scan-out period, some artifacts may appear on the screen. To avoid
this, we use `vertical synchronization` (v-sync or vsync): we only modify what
is displayed during the vertical blanking interval.

==== Multi-buffering

If we use a single framebuffer, modifying its contents only during the vertical
blanking interval may be difficult in practice as it imposes hard deadlines to
complete the modifications.

Instead, we can use several framebuffers:

- the *front buffer* that is currently used for display and that is not modified
- the *back buffer(s)* that can be modified

When a framebuffer is ready to be displayed, it becomes the "front buffer" and
the current front buffer becomes a "back buffer". The action of switching
framebuffers is called `page flipping`.

The page flipping must occur during the v-blank interval, otherwise `screen
tearing` may appear: some part of the displayed image is taken from the first
buffer and some other part from the second one.

If we want to start rendering a new frame without waiting for the page flipping
to complete, we can use more than one back buffer (e.g., `triple buffering`).

==== Synchronization issues

Rendering frames at the appropriate rate for the display may be difficult in
some cases:

- the frame is complex and the time to render it is longer than a vsync period
- the frame rate is imposed by the source (e.g., a video encoded at 24 FPS) and
  is not compatible with the display's refresh rate

===== Adaptive vertical synchronization

If the computer can't generate frames fast enough for some refresh rates (e.g.,
in a game), it may be better to temporarily disable vertical synchronization to
reduce the latency (hence avoiding stuttering) at the cost of some visual
artefacts. This is a trade-off between image quality and sufficiently fast frame
generation speed as you can't have both.

This is what NVIDIA names "adaptive vsync"
(http://www.geforce.com/hardware/technology/adaptive-vsync/technology>)

===== Incompatible rates

There are several techniques to try to deal with incompatible frame rates, such
as:

- if a frame is displayed during several periods, take this duration into
  account when rendering animated objects
- add motion blur to animated objects
- use interlaced frames

Further reading :

- https://blog.fishsoup.net/2011/06/22/what-to-do-if-you-cant-do-60fps/
- https://blog.fishsoup.net/2012/11/28/avoiding-jitter-in-composited-frame-display/
- https://en.wikipedia.org/wiki/Telecine#Frame_rate_differences
- https://en.wikipedia.org/wiki/Motion_interpolation


==== Programming

===== Events

The kernel can send events to user-space to signal the beginning of v-blank
periods and to signal the completion of page flips.


// TODO
//  A solution
//  - switch the source atomically
//  - v-blank events
//  Examples
//  - simple rendering engine
//  - link to the Clock example in appendices

// TODO
// - time to render a frame vs refresh period vs v-blank period
// - Explanation scan-out (flikering?)
// - Explanation multi-buffering
// - Code framebuffer swap ("page flip")
// - Explanation v-blank (tearing?)
// - Code synchro v-blank (event v-blank)
// - Note "async page flip" flag and "page flip complete" event
// - Adaptive v-sync
// - Dithering (frame rate control, TN panels 6-bits per RGB)
// - mode not supported by display device: garbage, black screen, error screen

==== Further reading

- https://en.wikipedia.org/wiki/Multiple_buffering#Page_flipping
- https://en.wikipedia.org/wiki/Vertical_blanking_interval
- https://en.wikipedia.org/wiki/Screen_tearing

- https://en.wikipedia.org/wiki/Multiple_buffering#Page_flipping
- https://en.wikipedia.org/wiki/Screen_tearing
- https://en.wikipedia.org/wiki/Vertical_blanking_interval
- https://en.wikipedia.org/wiki/Multiple_buffering#Triple_buffering
- https://en.wikipedia.org/wiki/Analog_television#Vertical_synchronization

=== GUI

Graphics User Interfaces (GUI) reflect the state of the system and let the user
interact with it. There are two things:

- Frames: pictures that are displayed on screen

- Interaction: given the currently displayed frame and the state of the
     system (i.e. the currently displayed frame is part of the state in fact),
     how to react to events (mouse click, key press, etc.)

==== Basic game loop

A basic game loop acts like this:

```haskell
gameLoop :: IO ()
gameLoop = do

  -- upate the state
  whileM (currentTime - lastSimulationTime >= timeStep) do
     -- process input events and other system events that have happened
     -- during [lastSimulationTime, lastSimulationTime+timeStep]
     processEvents

     updateSimulation timeStep  -- update physics in constant meaningful steps

     lastSimulationTime += timeStep

  -- perform frame rendering
  -- Parameter is used for interpolation (1 frame lag) or extrapolation
  -- (potential glitches) of the remaining time
  tmpState <- simulateUpdate (currentTime - lastSimulationTime)
  render tmpState

  unlessM quitTheGame
     gameLoop
```


==== Modifying the state

With MonadFlow M we track access to values from the Monad M. Basically we build
a tree of commands but if the values we read from M are the same as those in
cache, we know that the tree is exactly the same (pure function with the same
inputs).

We build a pure tree representing the new state or we indicate if no change
occurred: then we don't have to redraw at all.

M must be able to give a coherent view of the system at a given point in time.

==== References

- http://gameprogrammingpatterns.com/game-loop.html
- https://dewitters.com/dewitters-gameloop/
- https://webcache.googleusercontent.com/search?q=cache:5cH3UfBvb2YJ:vodacek.zvb.cz/archiv/681.html&hl=en&gl=us&strip=1&vwsrc=0
