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

// Page break before level 1 titles
#show heading.where(level:1): it => {
  pagebreak(weak: true)
  it
}

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
    title: "Haskus Computer Manual"
  , author: "Sylvain Henry"
  , date: auto
)

#set page(
    numbering: "1"
  , number-align: right
)

// Front page

#page(
  numbering: none,
  align(center,
    [
      #v(40%)
      #text(
      size: 26pt,
      [
      Haskus Computer Manual
      ]
    )\
    #v(5%)
    #text(
      size: 22pt,
      [
      Sylvain Henry
      ]
    )
    #v(5%)
    #text(
      size: 12pt,
      [
      Version: #datetime.today().display()
      ]
    )
  ]
  )
)

#outline()

#include "src/intro.typ"
#include "src/stdlib.typ"
#include "src/system.typ"
#include "src/compiler.typ"
#include "src/graphics.typ"

#bibliography(
    style: "ieee",
    "src/biblio.yml"
  )
