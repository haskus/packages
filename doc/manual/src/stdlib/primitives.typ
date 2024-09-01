== Primitives types

=== Haskell execution model

The Haskell execution model consists in having a *heap* where immutable
objects are allocated and automatically garbage-collected when they are no
longer used. A Haskell program consists in applying functions to these objects
to create new objects.

Haskell uses *lazy evaluation* by default: function applications are only
evaluated when needed, otherwise a function application is an object in the heap
called a *thunk*. A thunk references the function to execute and the argument
objects. When it is evaluated (i.e. executed), the thunk object is replaced with
a reference to the result of the computation (it is updated in place in the
heap). As such, a thunk is only evaluated once, which is the second aspect of
being lazy#footnote[If thunks were evaluated every time they are needed, the
language would only be said to be non-strict, not lazy.].

Each Haskell thread is asking for the evaluation of a specific function
appplication in the heap. For example, the one corresponding to the *main*
binding declaration in a program. Recursively, all the required evaluations will
be performed until the result is an object that can't be evaluated more.

Integrating side-effects in this model isn't trivial because there is no
evaluation order. A solution to this is to thread a value so that the execution
of a function needs to wait for another to pass it the token. This is the
purpose of the Haskell `State# s` primitive type passed between side-effecting
functions. By using values of this type we can express control dependencies
("call to function B must be executed after call to function A") as a data
dependency ("one of the arguments applied to B is a result of calling A, so A
must be called before B").

==== Making it fast

The execution model is nice in theory:
+ find the next function application or thunk to evaluate
+ evaluate it, allocating new objects in the heap for the results
+ update: replace the evaluated thunk with a reference to their result

However it is also very slow if implemented naively. The more objects get
allocated in the heap, the more often garbage collection must happen.

A lof of research around Haskell was about working around this execution model:

+ building a stack of functions to execute (called the "spine") instead of
  traversing the heap several times to find the next thing to evaluate.
+ if the thing we evaluate is only visible to the next thing to evaluate, then
  don't update the heap.
+ if the things that are evaluated in sequence are known at compilation time,
  generate some machine code equivalent to their evaluation ("supercombinators")
+ values that are passed between things evaluated in sequence don't need to pass
  through the heap; pass them via CPU registers and stack as much as possible
  ("unboxed types", "worker-wrapper" transformation, "constructed product result
  (CPR)" transformation...)
+ if the thing to execute next is allocated but is also known statically not to
  escape, "allocate" it in stack ("let-no-escape" and "join-points")
+ if a function argument is known to be evaluated every time in the function
  body, then evaluate it before the call and assume it to be evaluated in the
  function body ("demand analysis").

As laziness is implemented by allocating thunks in the heap, several of these
optimizations don't work when laziness is involved. As such, Haskell provides
some ways to disable laziness at specific places:
- bang patterns can be used to force a binding to be considered strict. E.g. a
  function argument (even if it is not used strictly in the function body).
- strict data constructor fields: constructor field that is guaranteed to
  pointing to an evaluated object (i.e. not a thunk).
- unpacked strict data constructor fields: constructor field that contains the
  contents of the evaluated object, not a pointer to it.

Instead of starting by exposing the ideal model and then learning the hard way
how to work around it, in this book we use a different approach: start with the
unboxed primitives first.

=== Primitive types

Haskell provides several primitives data types. These types are said to be
*unboxed*: they aren't allocated on the heap; values with these types are passed
in CPU registers and in the thread stacks.
