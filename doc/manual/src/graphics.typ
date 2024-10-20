= Computer Graphics <volume-graphics>

Computer Graphics is a vast topic. It encompasses most visual things we use a
computer for:

- image digitalization: what is a digital image and how do we store it
- image compression: how to compress digital images (with or without loss of
  information)
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

In physics there are two models to describe electromagnetic radiations:
- at our scale, we use the model of *electromagnetic waves*
- at the atomic scale, we use *quantum electrodynamics* (with photon "particles")

=== Electromagnetic waves

In the _electromagnetic waves_ model, an electromagnetic radiation is a periodic
oscillation of the electric and magnetic fields. These two fields affect each
other in a particular way described by Maxwell's equations and oscillate in
phase. However we don't need to dig that deep to understand colors! We only need
to know about some of the properties of these oscillations.

The main property of an electromagnetic radiation is its *frequency* usually
noted _f_ and expressed in _hertz_: it's the number of oscillations of the
fields per second.

The amplitudes of the electric and magnetic fields are also important
properties, but we won't use them directly. Instead we use them to derive the
_energy flux_ of the wave: the energy $S(x,t)$ transmitted by the wave at a
distance $x$ and at time $t$. However we don't even use the energy flux
directly. Instead we compute and (this time!) use an average of the energy flux
called the *Intensity*. Intensity is denoted _I_ and is expressed in watts per
squared meters ($W"/"m^2$).

#quote(block:true, attribution: [Section 16.4 of @university-physics-volume2: Energy Carried
by Electromagnetic Waves])[Because the frequency of visible light is very high,
of the order of $10^14$Hz, the energy flux for visible light through any area is
an extremely rapidly varying quantity. Most measuring devices, including our
eyes, detect only an average over many cycles. The time average of the energy
flux is the intensity _I_ of the electromagnetic wave and is the power per unit
area.]

The *speed of travel* of an electromagnetic radiation depends on the medium it
is travelling in and on the frequency _f_ of the electromagnetic radiation. We
denote it with $c_"medium" (f)$ where _c_ stands for _celeritas_ in Latin,
meaning _speed_. Speed of travel is expressed in meters per second ($m"/"s$).

Propagation in vacuum is a special case as the speed of travel doesn't depend on
the electromagnetic radiation frequency _f_. As such we can write $c_"vacuum" =
299,792,458 m"/"s$ for the well-known "speed of light" constant.The speed of
travel through matter is less than it is in a vacuum because the radiation
interacts with atoms in the material. Hence $c_"vacuum"$ is the upper limit for
the speed of an electromagnetic radiation.

To avoid manipulating large speed numbers, we prefer using the *refractive
index* $n$ of a medium, defined as:

$ n_"medium" (f) = c_"vacuum"/(c_"medium" (f)) >= 1 $

Similarly, to avoid manipulating large numbers for frequencies it is common to
use the wavelength $#sym.lambda$ defined as follow and to express it in a small
meter unit, e.g. nanometres ($"nm"$) for visible light:

$ #sym.lambda (f) = c_"vacuum" / f $

#figure(
  caption: [Refractive index of some media for some wavelengths.
  From Section 1.6 of @university-physics-volume3
  ],
  align(center,[
#table(
  columns: (auto, auto, auto, auto, auto, auto, auto),
  align: (left,center,center,center,center,center,center),
  table.header(
    [*Medium*], [*Red \ 660 nm*], [*Orange \ 610 nm*], [*Yellow \ 580 nm*],
    [*Green \ 550 nm*], [*Blue \ 470 nm*], [*Violet \ 410 nm*]
  ),
  [$n_"water"$], [1.331], [1.332], [1.333], [1.335], [1.338], [1.342],
  [$n_"diamond"$], [2.410], [2.415], [2.417], [2.426], [2.444], [2.458],
  [$n_"glass, crown"$], [1.512], [1.514], [1.518], [1.519], [1.524], [1.530],
  [$n_"glass, flint"$], [1.662], [1.665], [1.667], [1.674], [1.684], [1.698],
  [$n_"polystyrene"$], [1.488], [1.490], [1.492], [1.493], [1.499], [1.506],
  [$n_"quartz, fused"$], [1.455], [1.456], [1.458], [1.459], [1.462], [1.468],
)
]))


TODO:
- dispersion: angle of refraction
- attenuation: intensity attenuation through a medium. Also depends on the wave frequency
- another property: polarization
- spectrum

A thorough explanation of the origin of the refractive index is out of the scope
of this book. On this topic, the interested reader may read Richard Feynman
@feynman-lectures-31.



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
