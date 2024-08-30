== Writing data in memory

There are several strategies to write data into memory:

*Strategy 1: assume there is enough space and just write!*

The first strategy is to assume that there is enough memory space to write
the data we want. This happens for example:
  - when writing encoded x86 instructions: we know that they have a maximum length of 15 bytes
  - when writing an image into a buffer: we know our image dimensions so we
    can ensure that our buffers are large enough.

This is the fastest strategy because there isn't any boundary check occurring at
writing time. This strategy is implemented in `Haskus.Memory.Writer` (see
@fig-writer-def) by a function taking an address where to write and returning
the address after the bytes freshly written. The value to write is captured by
the function and not materialized in the types.

#figure(
    caption: "Haskus.Memory.Writer",
  [
```haskell
newtype Writer s
  = Writer (Addr# -> State# s -> (# State# s, Addr# #))

instance Semigroup (Writer s) where
  Writer f <> Writer g = Writer \addr0 s0 ->
    let !(# s1, addr1 #) = f addr0 s0
    in g addr1 s1

instance Monoid (Writer s) where
  mempty = Writer \addr s -> (# s, addr #)
```]
) <fig-writer-def>

`Writer` values can be composed easily and efficiently: see `Semigroup` and
`Monoid` instances in @fig-writer-def.

The `Haskus.Memory.Writer` module contains many `write*` functions to write
basic types using this method. Consider the example in @fig-writer-example which
uses some of them to write 3 values: "8" with 1 byte, "16" with 2 bytes, and 32
with 4 bytes. Compiling with optimization leads to the x86-64 machine code in
@fig-writer-example-asm which is efficient (no heap allocations, no jumps,
etc.).

#columns(2, [

  #figure(
    caption: "Writer example",
    ```haskell
     foo :: Writer s
     foo = mconcat
       [ writeU8  8
       , writeU16 16
       , writeU32 32
       ]
     ```
   ) <fig-writer-example>

  #colbreak()

  #figure(
    caption: [x86 assembly generated from @fig-writer-example],
    ```asm
    .globl foo1_info
    .type foo1_info, @function
    foo1_info:
            movb $8,(%r14)
            leaq 1(%r14),%rax
            movw $16,(%rax)
            addq $2,%rax
            movl $32,(%rax)
            leaq 4(%rax),%rbx
            jmp *(%rbp)
    ```
  ) <fig-writer-example-asm>
]
)


*Strategy 2: provide a way to check if there is enough space, then just write!*

If we know beforehand how much memory a writer requires, we can act if there
isn't enough space (e.g. allocating more memory, flushing a buffer to disk to
make some space, throwing an exception...).

This is implemented in `Haskus.Memory.Writer.SizedWriter`. A `SizedWriter`
carries a `U#` value (unsigned machine word) indicating the number of bytes that
would be written by the writer. The implementation of `SizedWriter` is isomorphic
to the one in @fig-sizedwriter-def-possible, but the one we use is in
@fig-sizedwriter-def. The latter is better because it help ensuring that no
`SizedWriter` value is ever allocated.

#figure(
    caption: "SizedWriter possible implementation",
    ```haskell
data SizedWriter s = SizedWriter
  { sizedWriterSize :: !U#
      -- ^ The number of bytes that will be written by the writer
  , sizedWriter     :: !(W.Writer s)
      -- ^ The Writer associated with this SizedWriter
  }
```
) <fig-sizedwriter-def-possible>

#figure(
    caption: "SizedWriter real implementation",
    ```haskell
newtype SizedWriter s
  = SizedWriter' ( (##) -> (# U#, W.Writer s #) )
```
) <fig-sizedwriter-def>

If we rewrite our previous example (@fig-writer-example) to use `SizedWriter`
instead of `Writer` (@fig-sizedwriter-example), we obtain exactly the same
x86-64 machine code for the writer part and the whole number of bytes to write
is statically computed (@fig-sizedwriter-example-asm).

#figure(
  caption: "SizedWriter example",
  ```haskell
   foo :: SizedWriter s
   foo = mconcat
     [ writeU8  8
     , writeU16 16
     , writeU32 32
     ]
   ```
 ) <fig-sizedwriter-example>

#figure(
    caption: [x86 assembly generated from @fig-sizedwriter-example],
    ```asm
.globl SizedWriter.foo1_info
.type SizedWriter.foo1_info, @function
SizedWriter.foo1_info:
	leaq SizedWriter.foo_w_closure+2(%rip),%r14 ; address of the writer closure
	movl $7,%ebx                                ; number of written bytes (7)
	jmp *(%rbp)
    ```
) <fig-sizedwriter-example-asm>

*Other strategies*

+ check available space, refuse to write if there isn't enough space
  - easy to implement continuation (call the same function again)
+ check available space, write as much as possible, return continuation
  - need to allocate a continuation, but do as much work as possible every time

*Cost of determining the required size*

Determining the number of bytes to write may be costly: e.g. to write elements
of a lazy list. In this case, just counting the bytes could force the elements
of the lazy list, blowing up memory. A better strategy in this case could be to
interleave byte counting with actually writing the bytes.
