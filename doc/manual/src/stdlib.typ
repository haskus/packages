#import "helpers.typ" as haskus

= Programming in Haskell

Most of the Haskus libraries are written in Haskell for now.
Haskell is the language the author is the most proficient with. Code written in
Haskell is easy to refactor and to maintain. Performance of Haskell programs is
good enough for most of our purposes (documenting and experimenting with computer
programming).

The Haskus standard library is a opiniated set of Haskell modules.  These are
"utility" modules in the sense that they aren't part of a specific domain:
common types (numbers, variants, etc.), common data structures (arrays, lists,
etc.), common functions (writing binary data into memory, etc.)...
These are used by other domain specific libraries (system programming, computer
graphics, etc.).

Note: when we write "Haskell", we mean "Haskell as implemented by a recent GHC".

#include "stdlib/primitives.typ"
#include "stdlib/numbers.typ"
#include "stdlib/variant.typ"
#include "stdlib/eadt.typ"
#include "stdlib/binary.typ"
#include "stdlib/writer.typ"
