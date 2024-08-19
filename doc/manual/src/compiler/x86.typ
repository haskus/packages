== x86 assembly

Notes on x86 in ``haskus-system`` and in general.

=== Instruction list

`haskus-system` aims to support X86 architectures by providing a declarative
list of instruction specifications (in `Haskus.Arch.X86_64.ISA.Insns.`) that
can be used to write:

- disassemblers
- assemblers
- analyzers
- (micro-)benchmarks
- emulators/simulators

Most instruction lists I have found (e.g., NASM's insns.dat file, Intel's
documentation) "flatten" the instruction polymorphism down to operand classes:
for each instruction, there is an entry in the list for each tuple of possible
operand classes. In ``haskus-system``, I have tried to keep as much of the
original semantics in a declarative way. For instance, I describe the semantics
of some optional bits in the opcodes:

- bit indicating that operands have to be commuted
- bit indicating that the operand size is 8-bit
- bit indicating that the FPU stack is popped
- etc.

In theory, it should be easily possible to generate a flattened instruction list
from `haskus-system`'s one, while the other way around is much more involved.

Additionally, the list contains for each instruction:

- flags that are read/set/unset/undefined
- implicit operands
- operand access mode: Read/Write/Both/None (used to access metadata)
- constraints: alignment requirement for memory
- supported prefixes
- required X86 extensions
- some documentation

In the future, I would like to add:

- pseudo-code for each instruction (useful for emulators, etc.)
- exceptions (maybe inferred from pseudo-code?)
- required privileges
- maybe precomputed latencies and such as found in Agner's work
  (http://www.agner.org) for different architectures (useful for
  cross-compilers) and a way to compute them ourselves on the current
  architecture

=== Instruction Operands

The polymorphism of x86 instructions is hard to formalize. For each operand, we
have to consider:

- the type of data it represents: an integer, a float, a vector of something
- its size
- where it is stored: in memory or in a register (which depends on the type and
  size)

There are often functional dependencies between operands: the type of one of the
operands may constrain the type of the other ones.

Usually assemblers infer the operand size from the operands, but as memory
addresses don't indicate the type of the data they point to, explicit
annotations are sometimes required.

=== Semantics vs Encoding

There isn't an isomorphism between an instruction and its encoding. Put another
way: sometimes it is possible to encode a given instruction using different
encodings. A basic example is to add superfluous prefixes (e.g., overriding the
default segment with the same segment).  The semantics of the program stays the
same but there are side-effects:

- instruction size vs instruction cache
- instruction alignment vs instruction decoder

An assembler would have to choose whether to use one encoding form or another.
In ``haskus-system``, I have tried to define an isomorphism between instruction
encodings and a more high-level instruction description by using encoding
variants (see ``EncodingVariant`` in ``Haskus.Arch.X86_64.ISA.Insn``). The
decoder indicates which variant of an instruction encoding it has decoded. The
(future) encoder will allow the user to specify which encoding to use (for
side-effect considerations).

=== Instruction entry point

x86 ISA uses some prefixes to alter the meaning of some instructions. In the
binary form (in the legacy encoding), a prefix is just a byte put before the
rest of the instruction bits.

It is possible to jump/branch into the instruction *after* some prefixes in
order to avoid them. Hence there is a kind of instruction overlay: two
"different" instructions sharing some bits in the binary. It makes the
representation and the analyze of these programs a little bit trickier.
Especially if we make a non-linear disassembly by following branchs: we get
branch labels inside instructions.

I have been told this is used in the glibc.

=== Encoding size

#image("images/x86-encoding.svg")
