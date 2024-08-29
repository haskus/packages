== Data representation

``haskus-base`` contains modules dedicated to the manipulation of binary data.
It also provides data type mapping those of other languages such as C and even
more.

Some packages (e.g. ``haskus-system``) use these binary modules to provide
bindings to C libraries. We don't want to rely on external tools such as C2HS to
provide bindings to C libraries because:

- We don't want to depend on .h files;
- .h files often contain pecularities that are difficult to handle
  automatically;
- Documentation and code of the resulting Haskell files are often very bad:

    - No haddock
    - Very low-level (e.g. `#define` are not transformed into ADTs with Enum
      instances)

Instead ``haskus-base`` lets you write bindings in pure Haskell code and
provides many useful things to make this process easy.

=== Word, Int

`Haskus.Format.Binary.Word` contains data types representing unsigned words
(`Word8`, `Word16`, `Word32`, etc.) and signed integers (`Int8`, `Int16`,
`Int32`, etc.). It also contains some C types such as `CSize`, `CShort`,
`CUShort`, `CLong`, `CULong`, etc.

==== Endianness

Words and Ints are stored (i.e., read and written) using host endianness (byte
ordering). `AsBigEndian` and `AsLittleEndian` data types in the
`Haskus.Format.Binary.Endianness` module allow you to force a different
endianness.

The following example shows a data type containing a field for each endianness
variant. We explain how to use this kind of data type as a C structure later in
this document.

```haskell
data Dummy = Dummy
  { fieldX :: Word32                -- ^ 32-byte unsigned word (host endianness)
  , fieldY :: AsBigEndian Word32    -- ^ 32-byte unsigned word (big-endian)
  , fieldZ :: AsLittleEndian Word32 -- ^ 32-byte unsigned word (little-endian)
  }
  deriving (Generic,Storable)
```

We can also explicitly change the endianness with the following methods:
`hostToBigEndian`, `hostToLittleEndian`, `bigEndianToHost`,
`littleEndianToHost`, `reverseBytes`.

Each of these methods is either equivalent to `id` or to `reverseBytes`
depending on the host endianness.

=== Structures (records)

You map C data structures with Haskell data type as follows:

```haskell
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

import Haskus.Format.Binary.Storable
import Haskus.Utils.Types.Generics (Generic)

data StructX = StructX
  { xField0 :: Word8
  , xField1 :: Word64
  }
  deriving (Show,Generic,Storable)
```


The Storable instance handles the alignment of the field as a C non-packed
structure would (i.e. there are 7 padding bytes between `xField0` and
`xField1`).

`peek` and `poke` can be used to read and write the data structure in memory.

==== Nesting

Data structures can be nested:

```haskell
data StructY = StructY
  { yField0 :: StructX
  , yField1 :: Word64
  }
  deriving (Show,Generic,Storable)
```

=== Arrays (vectors)

`haskus-base` supports vectors: a fixed amount of Storable data correctly
aligned. You can define a vector as follows:

```haskell
{-# LANGUAGE DataKinds #-}

import Haskus.Format.Binary.Vector as V

v :: Vector 5 Word16
```

Vectors are storable, so you can `peek` and `poke` them from memory.
Alternatively, you can create them from a list:

```haskell
Just v = fromList [1,2,3,4,5]
Just v = fromList [1,2,3,4,5,6] -- this fails dynamically
Just v = fromList [1,2,3,4]     -- this fails dynamically

-- take at most 5 elements then fill with 0: v = [1,2,3,4,5]
v = fromFilledList 0 [1,2,3,4,5,6]

-- take at most 5 elements then fill with 7: v = [1,2,3,7,7]
v = fromFilledList 7 [1,2,3]

-- take at most 4 (!) elements then fill with 0: v = [1,2,3,0,0]
v = fromFilledListZ 0 [1,2,3]

-- useful for zero-terminal strings: s = "too long \NUL"
s :: Vector 10 CChar
s = fromFilledListZ 0 (fmap castCharToCChar "too long string")
```

You can concatenate several vectors into a single one:

```haskell
import Haskus.Utils.HList

x = fromFilledList 0 [1,2,3,4] :: Vector 4 Int
y = fromFilledList 0 [5,6]     :: Vector 2 Int
z = fromFilledList 0 [7,8,9]   :: Vector 3 Int

v = V.concat (x `HCons` y `HCons` z `HCons` HNil)

>:t v
v :: Vector 9 Int

> v
fromList [1,2,3,4,5,6,7,8,9]
```

You can also safely `drop` or `take` elements in a vector. You can also `index`
into a vector:

```haskell
import Haskus.Format.Binary.Vector as V

v :: Vector 5 Int
v = fromFilledList 0 [1,2,3,4,5,6]

-- v2 = [1,2]
v2 = V.take @2 v

-- won't compile (8 > 5)
v2 = V.take @8 v

-- v2 = [3,4,5]
v2 = V.drop @2 v

-- x = 3
x = V.index @2 v
```

Finally, you can obtain a list of the values

```haskell
> V.toList v
[1,2,3,4,5]
```

=== Enums

If you have a C enum (or a set of `#define`'s) with consecutive values and
starting from 0, you can do:

```haskell
{-# LANGUAGE DeriveAnyClass #-}

import Haskus.Format.Binary.Enum

data MyEnum
  = MyEnumX
  | MyEnumY
  | MyEnumZ
  deriving (Show,Eq,Enum,CEnum)
```

If the values are not consecutive or don't start from 0, you can write your own
`CEnum` instance:

```haskell
-- Add 1 to the enum number to get the valid value
instance CEnum MyEnum where
  fromCEnum = (+1) . fromIntegral . fromEnum
  toCEnum   = toEnum . (\x -> x-1) . fromIntegral
```

To use an Enum as a field in a structure, use `EnumField`:

```haskell
data StructZ = StructZ
   { zField0 :: StructX
   , zField1 :: EnumField Word32 MyEnum
   }
   deriving (Show,Generic,Storable)
```

The first type parameter of `EnumField` indicates the backing word type (i.e. the
size of the field in the structure). For instance, you can use Word8, Word16,
Word32 and Word64.

To create or extract an `EnumField`, use the methods:

```haskell
fromEnumField :: CEnum a => EnumField b a -> a
toEnumField   :: CEnum a => a -> EnumField b a
```

We use a CEnum class that is very similar to Enum because Enum is a special
class that has access to data constructor tags. If we redefine Enum, we cannot
use `fromEnum` to get the data constructor tag.


=== Bit sets (or "flags")

We often use flags that are combined in a single word. Each flag is associated
to a bit of the word: if the bit is set the flag is active, otherwise the flag
isn't active.

`haskus-base` uses the `CBitSet` class to get the bit offset of each flag. By
default, it uses the Enum instance to get the bit offsets as in the following
example:

```haskell
{-# LANGUAGE DeriveAnyClass #-}

import Haskus.Format.Binary.BitSet

data Flag
  = FlagX  -- bit 0
  | FlagY  -- bit 1
  | FlagZ  -- bit 2
  deriving (Show,Eq,Enum,CBitSet)
```

If you want to use different bit offsets, you can define your own CBitSet
instance:

```haskell
-- Add 1 to the enum number to get the valid bit offset
instance CBitSet Flag where
  toBitOffset   = (+1) . fromEnum
  fromBitOffset = toEnum . (\x -> x-1)
```

To use a bit set as a field in a structure, use BitSet:

```haskell
data StructZ = StructZ
  { zField0 :: ...
  , zField1 :: BitSet Word32 Flag
  }
  deriving (Show,Generic,Storable)
```

The first type parameter of BitSet indicates the backing word type (i.e. the
size of the field in the structure). For instance, you can use Word8, Word16,
Word32 and Word64.

Use the following methods to manipulate the BitSet:

```haskell
fromBits     :: (CBitSet a, FiniteBits b) => b -> BitSet b a
toBits       :: (CBitSet a, FiniteBits b) => BitSet b a -> b
member       :: (CBitSet a, FiniteBits b) => BitSet b a -> a -> Bool
notMember    :: (CBitSet a, FiniteBits b) => BitSet b a -> a -> Bool
toList       :: (CBitSet a, FiniteBits b) => BitSet b a -> [a]
fromList     :: (CBitSet a, FiniteBits b, Foldable m) => m a -> BitSet b a
intersection :: FiniteBits b => BitSet b a -> BitSet b a -> BitSet b a
union        :: FiniteBits b => BitSet b a -> BitSet b a -> BitSet b a
```
 
Note that we don't check if bit offsets are outside of the backing word. You
have to choose a backing word that is large enough.


=== Unions

An union provides several ways to access the same buffer of memory. To use them
with `haskus-base`, you need to give the list of available representations
in a type as follows:

```haskell
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}

import Haskus.Format.Binary.Union

u :: Union [Word8, Word64, Vector 5 Word16]
```

Unions are storable so you can use them as fields in storable structures or
you can directly `peek`/`poke` them.

You can retrieve a member of the union with `fromUnion`.  The extracted type
must be a member of the union otherwise it won't compile.

```haskell
fromUnion u :: Word64
fromUnion u :: Word8
fromUnion u :: Vector 5 Word16
fromUnion u :: Word32 -- won't compile!
```

To create a new union from one of its member, use `toUnion` or `toUnionZero`.
The latter sets the remaining bytes of the buffer to 0. In the example, the
union uses 10 bytes (`5 * 2` for `Vector 5 Word16`) and we write 8 bytes
(`sizeOf Word64`) hence there are two bytes that can be left uninitialized
(`toUnion`) or set to 0 (`toUnionZero`).

```haskell
u :: Union [Word8,Word64,Vector 5 Word16]
u = toUnion (0x1122334455667788 :: Word64)

> print (fromUnion u :: Vector 5 Word16)
fromList [30600,21862,13124,4386,49850]

-- or
u = toUnionZero (0x1122334455667788 :: Word64)
> print (fromUnion u :: Vector 5 Word16)
fromList [30600,21862,13124,4386,0]
```


=== Bit fields

You may need to define bit fields over words. For instance, you can
have a Word16 split into 3 fields X, Y and Z composed of 5, 9 and 2 bits
respectively.

#table(
  columns: (auto, auto, auto, auto),
  table.header(
    [], [*X*], [*Y*], [*Z*]
  ),
  [`w :: Word16`],
  [0 0 0 0 0],
  [0 0 0 0 0 0 0 0 0],
  [0 0],
)

You define it as follows:

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

import Haskus.Format.Binary.BitField

w :: BitFields Word16 [ BitField 5 "X" Word8 
                      , BitField 9 "Y" Word16
                      , BitField 2 "Z" Word8
                      ]
w = BitFields 0x0102
```

Note that each field has its own associated type (e.g. Word8 for X and Z)
that must be large enough to hold the number of bits for the field.

Operations on BitFields expect that the cumulated size of the fields is equal
to the whole word size: use a padding field if necessary.

You can extract and update the value of a field by its name:

```haskell
x = extractField @"X" w
z = extractField @"Z" w
w' = updateField @"Y" 0x100 w
-- w' = 0x402

z = extractField @"XXX" w -- won't compile

w'' = withField @"Y" (+2) w
```

Fields can also be `BitSet` or `EnumField`:

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}

import Haskus.Format.Binary.BitField
import Haskus.Format.Binary.Enum
import Haskus.Format.Binary.BitSet

data A = A0 | A1 | A2 | A3 deriving (Show,Enum,CEnum)

data B = B0 | B1 deriving (Show,Enum,CBitSet)

w :: BitFields Word16 [ BitField 5 "X" (EnumField Word8 A)
                      , BitField 9 "Y" Word16
                      , BitField 2 "Z" (BitSet Word8 B)
                      ]
w = BitFields 0x1503
```

BitFields are storable and can be used in storable structures.

You can easily pattern-match on all the fields at the same time with
`matchFields` and `matchNamedFields`. It creates a tuple containing one value
(and its name with `matchNamedFields`) per field.

```haskell
> matchFields w
(EnumField A2,320,fromList [B0,B1])

> matchNamedFields  w
(("X",EnumField A2),("Y",320),("Z",fromList [B0,B1]))
```

== Explicit Memory Management

This document is about explicit ("manual") memory management in Haskell. It
starts from the very low level memory allocation and buffer management and then
builds up to "pointer" management, binary writer/reader monads, binary
serialization of Haskell data, large file mapping, embedding of large binary
blobs into applications, etc.

===  Introduction

==== Rationale

Haskell is a garbage-collected language and is known for its use of immutable
data: why would we like to manage memory explicitly? Here is a list of reasons:

1. Support for large data sets
2. Close to the metal: better control and usually faster operations
3. Interaction with the outside world
4. Live outside of the garbage-collector (no GC overhead)

==== Memory model

The memory model we consider is the
#link("https://en.wikipedia.org/wiki/Flat_memory_model")[flat memory model]. A
memory is an array of cells each containing one *bit*. A bit is a binary digit
(either 0 or 1). Instead of indexing each bit individually, the memory is
divided in coarser groups of bits. Nowadays most architectures pack bits in
groups of 8 bits (called "byte" or "Word8" in Haskell) so that's what we assume.

Consecutive bytes are assigned consecutive addresses: an *address* is a number
(starting from 0) which maximal value depends on the architectures. For example,
on X86_64 architectures an address is stored in a 64-bit word but only the 48
lower bits are used.

Even if the whole memory is addressable, some addresses can't be given as
parameters to reading/writing instructions depending on the execution context
(state of the memory, state of the processes, etc.). Otherwise an
error/exception/interruption may be triggered or the result may be undefined.

This memory model is abstract: it can be applied to physical memory or to
virtual memory managed by some software (operating systems, system libraries,
etc.).

==== Memory allocation <memory-alloc>

Nowadays most mainstream systems and architectures require the use of a memory
allocator. Applications can't just read or write random addresses in memory
otherwise it could lead to data corruption or to prohibited access errors.
Instead programs must reclaim a chunk of consecutive cells (i.e. a
*buffer*) to the memory allocator.

Note: a memory allocator is also abstract: its facilities can be provided by the
hardware (less common), by the operating system (usually not used directly) or
by some "system libraries" built on the the operating system allocator (most
common).

The memory allocator tries to find a suitable set of consecutive memory cells
that fulfills the given constraints (minimal number of cells, address constraints
such as alignment or maximal value, etc.). It makes sure that these cells are
now accessible to the calling code and returns the address of the first cell to
the calling code.

Programs can release buffers that are no longer used. Memory cells of released
buffers can be used by the memory allocator for future memory allocations.

Allocating and releasing buffers can lead to memory
#link("https://en.wikipedia.org/wiki/Fragmentation_(computing)")[fragmentation]:
there are enough free memory cells to allocate but these are not consecutive
because allocated memory cells are still alive between them.

==== GHC memory allocation <memory-alloc-ghc>

GHC provides its own memory allocator on top of the system one. It is well
integrated with the copying garbage collector.

The main difference with the system one is that there is an additional
indirection: instead of returning the address of the first allocated memory
cell, GHC's memory allocator returns an abstract object that contains a mutable
memory address. GHC can freely copy the contents of the buffer to another
location and update the address in the abstract object. This avoids
fragmentation at the cost of buffer copying.

As copying buffers can be too costly, GHC's memory allocator also supports the
allocation of *pinned* buffers: these buffers are guaranteed not to be moved,
exactly like system allocated buffers. As an optimization, GHC automatically
considers large buffers as pinned buffers.

GHC also distinguishes at the type level buffers whose cells are mutable or
immutable. Immutable buffers can be shared, duplicated at will, etc. Mutable
buffers are cheaper to use as we don't have to duplicate the whole buffer to
change one cell as it would be the case with immutable buffers. Immutable
buffers are created by "freezing" mutable buffers: by convention a frozen
mutable buffer mustn't be used anymore (linear types may help to ensure this
statically in the future).

GHC allocated buffers are automatically released by the garbage collector.

Note: memory allocation in GHC is done with `MutableByteArray#` and `ByteArray#`
data types and their primitives.

// ==============================================================================
// State of the art
// ==============================================================================
// 
// State of the art as of January 14, 2019.
// 
// * Data.ByteString.Short:
//    ShortByteString ~ BufferI
// 
// * Data.ByteString.Strict:
//    ByteString ~ BufferSlice ({-# UNPACK #-} !BufferP)
// 
// 
// * ByteString: misleading name (not a textual string). Especially because of
//    Data.ByteString.Char8.{pack/unpack}
// 
// 
// * Data.ByteString.Lazy: BufferList BufferP
//    Lazy IO: quite bad
//    Fixed chunck size
// 
// * ByteString: embedding as literal strings (slow, don't support \0 in the
//   buffer)
// 
// * ByteArray, MutableByteArray
// 
// * Ptr, ForeignPtr: don't track buffer size

=== Buffer <buffer>

A `Buffer` object represents some allocated memory, i.e. a set of consecutive
memory cells. It has the following type:

```haskell
data Buffer (mut :: Mutability) (pin :: Pinning) (fin :: Finalization) (heap :: Heap)
```

It's a GADT whose parameters have the following meaning:

```haskell
-- | Is the buffer mutable or not?
data Mutability
   = Mutable   -- ^ Memory cells are mutable
   | Immutable -- ^ Memory cells are immutable

-- | Is the buffer pinned into memory?
data Pinning
   = Pinned    -- ^ The buffer has a fixed associated memory address
   | NotPinned -- ^ The buffer contents can be freely moved to another address

-- | Is the buffer automatically garbage collected?
data Finalization
   = Collected    -- ^ Automatically collected by the garbage-collector
   | Finalized    -- ^ Finalizers are run just before GCing
   | NotFinalized -- ^ Not managed at all

-- | Allocation heap
data Heap
   = Internal -- ^ GHC heap
   | External -- ^ External heap
```

If you have read the sections about memory allocation in general (@memory-alloc)
and  memory allocation in GHC (@memory-alloc-ghc), most parameters must be self
explanatory, except for `Finalized` explained below (@buffer-finalizer).

We define the following type aliases for the different buffer variants:

```haskell
type BufferI   = Buffer 'Immutable 'NotPinned 'Collected    'Internal
type BufferP   = Buffer 'Immutable 'Pinned    'Collected    'Internal
type BufferM   = Buffer 'Mutable   'NotPinned 'Collected    'Internal
type BufferMP  = Buffer 'Mutable   'Pinned    'Collected    'Internal
type BufferME  = Buffer 'Mutable   'Pinned    'NotFinalized 'External
type BufferE   = Buffer 'Immutable 'Pinned    'NotFinalized 'External
type BufferF   = Buffer 'Immutable 'NotPinned 'Finalized    'Internal
type BufferPF  = Buffer 'Immutable 'Pinned    'Finalized    'Internal
type BufferMF  = Buffer 'Mutable   'NotPinned 'Finalized    'Internal
type BufferMPF = Buffer 'Mutable   'Pinned    'Finalized    'Internal
type BufferMEF = Buffer 'Mutable   'Pinned    'Finalized    'External
type BufferEF  = Buffer 'Immutable 'Pinned    'Finalized    'External
```

==== Buffer size

Buffer size can be queried with:

```haskell
bufferSizeIO :: MonadIO m => Buffer mut pin fin heap -> m Word
bufferSize   :: BufferSize a => a -> Word
```

`bufferSize` is pure and as such it can't be used with internal mutable buffers
which can be resized. All the other buffers provide a `BufferSize` instance.


==== Finalizers <buffer-finalizer>

A finalizer is just a side-effecting function that can be associated to a data.
When a data is to be collected by GHC's garbage collector, GHC executes its
associated finalizers if the data has some before releasing the data.  This
mechanism is especially useful in the domain of explicit memory management (e.g.
to release externally allocated memory), hence `Buffers` can be `Finalized`
to ease the use of this mechanism. In particular it ensures execution order of
the finalizers by explicitly storing them in a list.

We can make any buffer `Finalized` with the following function (idempotent for
already `Finalized` buffers):

```haskell
makeFinalizable :: MonadIO m => Buffer mut pin f heap -> m (Buffer mut pin 'Finalized heap)
```

Then you can attach a finalizer with:

```haskell
addFinalizer :: MonadIO m => Buffer mut pin 'Finalized heap -> IO () -> m ()
```

The latest added finalizers are executed first. Note that finalizers are not
guaranteed to run (e.g. if the program exits before the buffer is collected).

==== Allocation

===== Allocation in GHC heap

Buffers allocated in GHC heap can be pinned or not. They are automatically
collected.

```haskell
newBuffer              :: MonadIO m => Word -> m BufferM
newPinnedBuffer        :: MonadIO m => Word -> m BufferMP
newAlignedPinnedBuffer :: MonadIO m => Word -> Word -> m BufferMP
```

`newAlignedPinnedBuffer` takes an additional alignement requirement.

===== Allocation using system malloc

Buffers allocated by system "malloc" allocator are pinned and must be either
explicitly freed with `Malloc.freeBuffer` or can be automatically freed by a
finalizer (e.g. if they are allocated with `Malloc.newFinalizedBuffer`).

```haskell
import qualified Haskus.Memory.Allocator.Malloc as Malloc

Malloc.newBuffer          :: MonadIO m => Word -> m (Maybe BufferME)
Malloc.newFinalizedBuffer :: MonadIO m => Word -> m (Maybe BufferMEF)
Malloc.freeBuffer         :: MonadIO m => BufferME -> m ()
```

===== Buffer freezing/thawing

Some buffers can be converted from mutable to immutable and vice versa. This is
unsafe as the original buffer mustn't be used anymore after this and this is not
statically checked by the compiler.

```haskell
-- | Mutable to immutable
unsafeBufferFreeze :: (Freezable a b, MonadIO m) => a -> m b

-- | Immutable to mutable
unsafeBufferThaw   :: (Thawable a b , MonadIO m) => a -> m b
```

==== Read/write

Several primitives are provided to read and to write buffer contents. Some
primitives have constraints on the buffer type to restrict their use. For
example, the type system ensures that we don't use writing primitives with
immutable buffers.

===== Reading/writing Word8

We can read a `Word8` value by providing an index/offset into the buffer:

```haskell
bufferReadWord8IO :: MonadIO m => Buffer mut pin fin heap -> Word -> m Word8
```

This is done in the IO monad because the function is generic and supports both
mutable and immutable buffers. If we deal with immutable buffers, we can use the
following pure function instead:

```haskell
bufferReadWord8 :: Buffer 'Immutable pin fin heap -> Word -> Word8
```

We can also write `Word8` into mutable buffers with:

```haskell
bufferWriteWord8IO :: MonadIO m => Buffer 'Mutable pin fin heap -> Word -> Word8 -> m ()
```

===== Reading/writing Word16/Word32/Word64

Reading and writing `Word16`, `Word32` or `Word64` could be expressed with
the primitives to read/write `Word8`. However, most architectures provide
instructions to directly read/write larger words. Using them is much more
efficient than falling back to `Word8` primitives. Hence buffers support the
following primitives too:

```haskell
bufferReadWord16 :: Buffer 'Immutable pin fin heap -> Word -> Word16
bufferReadWord32 :: Buffer 'Immutable pin fin heap -> Word -> Word32
bufferReadWord64 :: Buffer 'Immutable pin fin heap -> Word -> Word64

bufferReadWord16IO :: MonadIO m => Buffer mut pin fin heap -> Word -> m Word16
bufferReadWord32IO :: MonadIO m => Buffer mut pin fin heap -> Word -> m Word32
bufferReadWord64IO :: MonadIO m => Buffer mut pin fin heap -> Word -> m Word64

bufferWriteWord16IO :: MonadIO m => Buffer 'Mutable pin fin heap -> Word -> Word16 -> m ()
bufferWriteWord32IO :: MonadIO m => Buffer 'Mutable pin fin heap -> Word -> Word32 -> m ()
bufferWriteWord64IO :: MonadIO m => Buffer 'Mutable pin fin heap -> Word -> Word64 -> m ()
```

Different architectures store the `Word8` s composing larger words in different
orders (called `Endianness`). When we use buffers to exchange data with other
systems, we need to be aware of the endianness convention used for the exchanged
data. More on this in the following chapters.

===== Using the address of pinned buffers

Pinned buffers have a fixed associated memory address. We can use the following
functions to read or write a mutable pinned buffer by using primitives for
`Addr#` or `Ptr`:

```haskell
withBufferAddr# :: MonadIO m => Buffer 'Mutable 'Pinned fin heap -> (Addr# -> m a) -> m a
withBufferPtr   :: MonadIO m => Buffer 'Mutable 'Pinned fin heap -> (Ptr b -> m a) -> m a
```

Similarly we can do the same thing with immutable buffers with the following
functions:

```haskell
unsafeWithBufferAddr# :: MonadIO m => Buffer mut 'Pinned fin heap -> (Addr# -> m a) -> m a
unsafeWithBufferPtr   :: MonadIO m => Buffer mut 'Pinned fin heap -> (Ptr b -> m a) -> m a
```

The difference in this case is that we mustn't use the memory writing primitives
of `Addr#` and `Ptr` when the buffer is immutable as it would break referential
transparency, hence the "unsafe" prefix.

==== Copy

We can copy data from one buffer to another with:

```haskell
copyBuffer ::
  MonadIO m
  => Buffer mut pin0 fin0 heap0        -- ^ Source buffer
  -> Word                              -- ^ Offset in source buffer
  -> Buffer 'Mutable pin1 fin1 heap1   -- ^ Target buffer
  -> Word                              -- ^ Offset in target buffer
  -> Word                              -- ^ Number of Word8 to copy
  -> m ()
```

==== Performance

To enhance performance of the code using `Buffer`, most functions have been
specialized with the `SPECIALIZE INLINE` pragma so that if your code uses a
specific buffer type (e.g. `BufferI`, `BufferM`...) it is as if the
generic `Buffer` GADT didn't exist.

You can use it in your own generic code. Example:

```haskell
{-# SPECIALIZE INLINE bufferReadWord8IO :: MonadIO m => BufferI  -> Word -> m Word8 #-}
{-# SPECIALIZE INLINE bufferReadWord8IO :: MonadIO m => BufferP  -> Word -> m Word8 #-}
```


=== Literals and embedding

Sometimes we know at compile time what the (initial) contents of a Buffer
(@buffer) is. It would be cumbersome to have to allocate the buffer and to write
its content word by word at program initialization or before using the buffer.
This chapter presents the alternatives.

==== List literals

If the buffer is very small, we can use the `OverloadedLists` extension to
create an immutable buffer from a list of bytes.  It allows the creation of
small unpinned immutable buffers into GHC's heap (aka `BufferI`).

```haskell
{-# LANGUAGE OverloadedLists #-}

b :: BufferI
b = [25,26,27,28] -- Word8 values
```

This should only be used for very small buffers. First, because it is not the
most efficient way to build a buffer: the actual buffer will be created when it
is first used. Second, because it is very cumbersome to list bytes' values in a
list.

=== Embedding files as buffers

You can embed an external file (or some part of it) into your executable. At
runtime you can access it as a normal external buffer (mutable or not).

```haskell
{-# LANGUAGE TemplateHaskell #-}

import Haskus.Memory.Buffer
import Haskus.Memory.Embed

let b = $(embedFile "myfile.bin"
            True       -- is the resulting buffer mutable or not
            (Just 8)   -- optional alignment constraint
            (Just 10)  -- optional offset in the file
            (Just 128) -- optional number of bytes to include
         )
```

=== Embedding buffers using Template Haskell

If you know how to build your buffer at compile time, you can build it and embed
it into the executable with Template Haskell by using `embedBuffer`.

```haskell
import Haskus.Memory.Embed

embedBuffer
   :: Buffer mut pin fin heap -- ^ Source buffer
   -> Bool       -- ^ Should the embedded buffer be mutable or not
   -> Maybe Word -- ^ Optional alignement constraint
   -> Maybe Word -- ^ Optional offset in the source buffer
   -> Maybe Word -- ^ Optional number of bytes to include
   -> Q Exp      -- ^ BufferE or BufferME, depending on mutability parameter
```

=== Views <buffer-views>

Views can be used to refer to a subset of the cells of a buffer.

==== View patterns

The cells referred by a view may not be contiguous. Currently we support the
following patterns:

```haskell
-- | A view pattern
data ViewPattern
   = PatternFull  -- ^ The whole buffer
   | Pattern1D    -- ^ 1D slice
      { pattern1DOffset :: {-# UNPACK #-} !Word -- ^ Offset of the first cell
      , pattern1DSize   :: {-# UNPACK #-} !Word -- ^ Number of cells
      }
   | Pattern2D    -- ^ 2D slice
      { pattern2DOffset :: {-# UNPACK #-} !Word -- ^ Offset of the first line
      , pattern2DWidth  :: {-# UNPACK #-} !Word -- ^ Width (line size)
      , pattern2DHeight :: {-# UNPACK #-} !Word -- ^ Height (number of lines)
      , pattern2DStride :: {-# UNPACK #-} !Word -- ^ Stride (space between two lines)
      }
   | PatternOn ViewPattern ViewPattern -- ^ Composed pattern
```

==== View sources

A view can use one of the following sources: a (strong) buffer, a weak buffer or
a weak view. 

Strong views keep the underlying buffer alive, while weak views allow the source
to be garbage collected.

1. The source is a buffer. The view keeps the buffer alive

2. The source is a weak buffer. If the buffer is collected, its contents
   is copied into a new buffer and the view is updated to use it.

3. The source is a weak view. If the source view is collected, the
   current view is updated to use whatever the source view uses as a
   source (another view or a buffer).
   This mechanism makes buffer contents cascade into smaller views while
   preserving some sharing.


==== Weak views

Weak views are used so that the underlying buffer can be freed by the GC.
When it happens and if the view is still alive the contents of the buffer
used by the view is copied into a fresh (usually smaller) buffer.

Suppose we have a big buffer B. We can have buffer views on B, say vb1 and vb2.

```
B <----- vb1
^------- vb2
```

These views don't duplicate B's contents and they keep B alive.
If the views are much smaller than B, it may not be what we want: a lot of
space is wasted and we would better duplicate B's data required by the views
and free B.

To support this, we can use "weak buffer views", say wbv1 and wbv2.

```
B <~~~~~ wbv1
^~~~~~~~ wbv2
```

If/when B is collected, new buffers are created from it for the views:

```
B1 <----- wbv1
B2 <----- wbv2
```

We can also create "weak view views", say wvv1 and wvv2:

```
B <~~~~~ wvb1 <~~~~~ wvv1
           ^~~~~~~~~ wvv2
```

If/when B is collected before wvb1, the sharing is kept while the required
contents of B is duplicated:

```
B' <---- wbv1 <~~~~~ wvv1
           ^~~~~~~~~ wvv2
```

When wbv1 is collected, we can be in one of the following state depending if
B has been collected already or not:

```
B <~~~~~~~~~~~~~~~~~ wvv1
^~~~~~~~~~~~~~~~~~~~ wvv2

           B' <~~~~~ wvv1
           ^~~~~~~~~ wvv2
```

==== Example

```haskell
> :set -XOverloadedLists
> import System.Mem
> v <- newBufferWeakView ([10,11,12,13,14,15,16,17] :: BufferI) (Pattern1D 2 4)
> v2 <- newViewWeakView v (Pattern1D 1 1)
> putStr =<< showViewState v2
View source: weak view
Source size: 4
View pattern: Pattern1D {pattern1DOffset = 1, pattern1DSize = 1}
Wasted space: 75%
Source:
   View source: weak buffer
   Source size: 8
   View pattern: Pattern1D {pattern1DOffset = 2, pattern1DSize = 4}
   Wasted space: 50%

> performGC

> putStr =<< showViewState v2
View source: weak view
Source size: 4
View pattern: Pattern1D {pattern1DOffset = 1, pattern1DSize = 1}
Wasted space: 75%
Source:
   View source: buffer       -- the source of v (a weak buffer of size 8) has
   Source size: 4            -- been replaced by a strong buffer of size 4 when
   View pattern: PatternFull -- the source has been collected
   Wasted space: 0%
```

```haskell
> v <- (`newViewWeakView` Pattern1D 1 2) =<< newBufferWeakView ([10,11,12,13,14,15,16,17] :: BufferI) (PatternFull)

> putStr =<< showViewState v
Source size: 8
View pattern: Pattern1D {pattern1DOffset = 1, pattern1DSize = 2}
Wasted space: 75%
Source:
   View source: weak buffer
   Source size: 8
   View pattern: PatternFull
   Wasted space: 0%

> performGC

> putStr =<< showViewState v
View source: buffer
Source size: 2
View pattern: PatternFull
Wasted space: 0%
```
