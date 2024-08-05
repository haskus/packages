= Computer Graphics <graphics>

Computer Graphics is a vast topic. It encompasses most visual things we use a
computer for:

- image digitalization: what is a digital image and how do we store it
- image compression: how to compress digital images (with or without loss of
  information
- image printing
- image display: video display technologies
- image processing: transforming images
- image analysis and computer vision: analyzing images
- image generation: generating images from a formal description (ray-tracing, etc.)
- image editing: providing interactive user interfaces for image manipulation
  using virtual tools

If you're already a programmer, it's very likely that you've already read about
RGB colors: with just three numbers (quantity of red, quantity of green,
quantity of blue), we can describe any color (for example on a web page). If
you're curious you may ask yourself: how is that possible? Why red, green, and
blue? Are there other colors that can't be represented with this triplet?

These are all valid questions. In this book we don't skim over the explanations
about the physics of light and the biology of our light sensors (our eyes).
These information are required for other topics such as image compression and
physically-based image generation (e.g. photon mapping).

Be warned that if you go too deep into this rabbit-hole, you'll quickly
encounter quantum mechanics and get out with even more questions than you
started with.

== The physics of light

It so happens that our world is surrounded by *electromagnetic radiations* and
they are important for our topic because some of them are what we call "light".

Electromagnetic radiations are produced by the Sun, by fire, by lamps, by
microwave ovens, etc. They travel through space: in vacuum, in the air, to some
degree into water, glass and other materials. They can bounce on surfaces
(objects, people, particles...) or be absorbed by them. Ultimately some of them
may reach our eyes. Some cells in our eyes react to some particular
electromagnetic radiations and transmit signals to our brain that are
interpreted as colors.

#figure(
  caption: "A painting depicting some electromagnetic radiations from the Sun",
  image(
    width: 60%,
    "graphics/images/sunset.png"
  )
)

In physics there are two common models to describe electromagnetic radiations,
their travels and their interactions:
- at our scale, we can use the model of *electromagnetic waves*
- at the atomic scale, *quantum electrodynamics* (with photon particles) is a
  better model

=== Electromagnetic waves

The speed of an electromagnetic radiation is determined by the medium/material
it is in. In vacuum, this speed is 299,792,458 m/s (the well known "speed of
light" constant, denoted with $c_0$). $c_0$ is the upper limit for the speed of an
electromagnetic radiation.

TODO: optical frequency $v$ is constant for a monochromatic wave.
https://www.rp-photonics.com/wavelength.html

To avoid manipulating large speed numbers, the speed $v$ in other materials is
often given with the *refractive index* $n$ of the material, defined as: $n =
c_0/v$


#table(
  columns: (auto, auto, auto),
  align: (left,center,right),
  table.header(
    [*Material*], [*Refractive index $n$*], [*Phase velocity (m/s)* $v$]
  ),
  [Vacuum], [1], [299,792,458],
  [Glass], [1,5], [\~200,000,000],
)




=== Human Vision


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


=== Video Display

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

==== Refresh rates

The number of images (or frames) sent to the video display depends on its
refresh rate. Most video displays have a fixed (configurable) refresh rate. For
instance 60Hz (60 frames per second).

Newer video displays may also support variable refresh rates where the delay
between two frames may depend on the next frame being ready (or a timeout if it
isn't ready for too long).

==== Switching between frames <graphics-vsync>

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

==== Plane composition

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

== Further reading

- The Computer Graphics Manual (David Salomon)
