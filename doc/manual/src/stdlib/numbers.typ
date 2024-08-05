== Numbers

=== Natural numbers

Natural numbers are non-negative integers (0, 1, 2...). If we use a finite
numbers of bits to encode natural numbers, we can only encode a subset of the
infinite natural number set.

#table(
  columns: (auto, auto),
  table.header(
    [*#sym.hash bits*], [*Natural number range*]
  ),
  [1], [[$0$..$1$]],
  [2], [[$0$..$3$]],
  [3], [[$0$..$7$]],
  [n], [[$0$..$2^n-1$]],
)

`BitNat n` is the type of natural numbers encoded on `n` bits. We can define such
numbers as follow:

```haskell
import Haskus.Format.Number

> let x = BitNat @5 3
> :t x
x :: BitNat 5
```

Note that if we try to encode an out-of-range natural, it fails:

```haskell
> BitNat @50 10000000000000000000000
BitNat @50 *** Exception: `10000000000000000000000` is out of the range of values
that can be encoded by a 50-bit natural number: [0..1125899906842623]
```

Operations on natural numbers track the numbers of bits:

```haskell
> BitNat @4 10 .+. BitNat @2 1
BitNat @5 11

> BitNat @4 10 .*. BitNat @3 2
BitNat @7 20

>  BitNat @5 25 .-. BitNat @2 3
Just (BitNat @5 22)

> BitNat @5 2 .-. BitNat @2 3
Nothing  -- not a Natural (negative number)

> BitNat @5 25 ./. BitNat @2 3
Just (BitNat @5 8,BitNat @2 1) -- 25 = 8*3 + 1
```

Note that we track the statically determined number of bits, not the optimal
one! For example, in `BitNat @4 10 .+. BitNat @2 1 ==> BitNat @5 11` the
result is `11` which can be encoded with a 4-bit natural but the return type
is a 5-bit natural because adding a 4-bit natural and a 2-bit natural *may*
result in a 5-bit natural: e.g. `BitNat @4 15 .+. BitNat @2 1 ==> BitNat @5
16` and 16 can't be encoded with a 4-bit natural.

Explicitly specifying the number of bits required to store a literal natural
value can be cumbersome so we can make GHC do it for us with the `nat`
function:

```haskell
> nat @0
BitNat @1 0

> nat @5
BitNat @3 5

> nat @158748521123465897456465
BitNat @78 158748521123465897456465
```

=== Natural ranges

Sometimes we know that the natural numbers that we manipulate are in a fixed
range:

- An hour is in the range [1..12]
- The age of an adult is in the range [18..150]

We can use a natural range to store those values: it ensures that the value is
in the range and it uses just the necessary bits to store (max-min+1) values.

```haskell
> type Age = NatRange 18 150
> natRange @25 :: Age
NatRange @18 @150 25

> natRange @16 :: Age
error: 16 isn't in the range [18,150]
```

Operations on natural ranges track range boundaries:

```haskell
> NatRange @2 @4 3 .++. NatRange @7 @17 13
NatRange @9 @21 16
```


// TODO: Unums
// #### Compute tables
// 
// We should find ways to automatically populate look-up tables for all operations.
// 
// 
// ### SORN
// 
// In Unums v2.0, SORN are implemented as bit sets: 1 bit per Unum. For a n bits
// Unum, the SORN is 2^n large.
// 
// E.g., 8-bit  unum means 256-bit        SORN
//       16-bit unum means 65536-bit      SORN (8 kB)
//       24-bit unum means 16777216-bit   SORN (2 MB)
//       32-bit unum means 4294967296-bit SORN (512 MB)
// 
// Pros:
//    * manipulation is easy (bit operations)
//    * manipulation time is fast and constant (no indirection, etc.)
//    * precise set of Unum values
// Cons:
//    * size may be too big (especially in look-up tables: 2^(3n) bits for a
// full table)
// 
// We could find other SORN implementations with different trade-offs.
// 
// #### Contiguous SORN 
// 
// We can encode contiguous SORN with two values:
//    * start: the starting unum
//    * count: the number of unums from start upwards
// 
// Pros:
//    * size is much smaller (2 * unum size),  especially for look-up tables because
//    connected sets remain connected under addition, subtraction, multiplication
//    and division.
//    * trivial logic for negate and reciprocate (i.e., operate on bounds only)
// Cons:
//    * logic is a little bit more complicated because we have to mix up connected and disjoint sets
// 
// #### SORN as bloom-filters
// 
// In many cases, we can use Unums and SORN to reduce a search space: the computed
// solution gives us a SORN containing the solution and we may need to refine each
// Unum in the SORN to find it precisely.
// 
// In these case, we could use a bloom-filter as an implementation for the SORN: it
// would potentially contain false positives but not false negative.
// 
// Pros: much smaller SORN size (in bits)
// Cons: potential false positives

