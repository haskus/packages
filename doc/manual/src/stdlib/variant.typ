#import "../helpers.typ" as haskus

== Sum types: Variant <variant>

`V` (for `Variant`) is a sum type, i.e. a wrapper for a value which can be of
different types. For instance in the following code `x` is a variant whose value
can be an `Int`, a `Float` or a `String`:

```haskell   
import Haskus.Data.Variant
   
x :: V [Int,Float,String]
```

We use a type-level list of types to statically constrain the possible value
types. Compared to usual sum types (e.g. `Either Int Float`) it allows us to
have variants which can contain any number of types and to manipulate
(extend/filter/etc.) the list in a type-safe way and without requiring new data
types.

*See also*

- `VEither` (@veither) is a variant biased towards the first type in the list,
just like `Either a b` is biased towards the second type (`b`), allowing
instances such as `instance Functor (VEither a)` which we don't have for the `V`
variant type.

- recursive sum types based on ``Variant`` are also supported and are called
EADT (@eadt)


=== Why do we need Variant?

In the functional programming world we use algebraic data types (ADT), more
specifically #link("https://en.wikipedia.org/wiki/Tagged_union")[sum types], to
indicate that a value can be of two or more different types:

```haskell
x,y :: Either String Int
x = Left "yo"
y = Right 10
```

What if we want to support more than two types?

*Solution 1: sum types*

We could use different sum types with different constructors for each arity
(number of different types that the value can have).

```haskell
data SumOf3 a b c   = S3_0 a | S3_1 b | S3_2 c
data SumOf4 a b c d = S4_0 a | S4_1 b | S4_2 c | S4_3 d
```

But it's quite hard to work with that many different types and constructors as
we can't easily define generic functions working on different sum types without
a combinatorial explosion.

*Solution 2: recursive ADT*

Instead of adding new sum types we can use a nest of `Either`:

```haskell
type SumOf3 a b c   = Either a (Either b c)
type SumOf4 a b c d = Either a (Either b (Either c d))
```


Or more generically:

```haskell
data Union (as :: [*]) where
  Union :: Either (Union as) a -> Union (a : as)
```

This time we can define generic functions without risking a combinatorial
explosion. The drawback however is that we have changed the representation:
instead of `tag + value` where `tag` is in the range [0,arity-1] we have a
nest of `tag + (tag + (... (tag + value)))` where `tag` is in the range
[0,1]. It is both inefficient in space and in time (accessing the tag value is
in O(arity)).

*Solution 3: variant*

`Variant` gets the best of both approaches: it has the generic interface of
the "recursive ADT" solution and the efficient representation of the "sum types"
solution.

```haskell
data Variant (types :: [*]) = Variant {-# UNPACK #-} !Word Any

type role Variant representational
```

The efficient representation is ensured by the definition of the `Variant`
datatype: an unpacked `Word` for the tag and a "pointer" to the value.

The phantom type list `types` contains the list of possible types for the value.
The tag value is used as an index into this list to know the effective type of the
value.

=== How to use Variants?

To use `Variant`:

- add a dependency to the
#link("https://hackage.haskell.org/package/haskus-utils-variant")[haskus-utils-variant]>
package
- use the following import: `import Haskus.Data.Variant`

You may need to enable some language extensions:

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
```

=== How to set create Variant values?

The easiest way to create a variant value is to use the `V` pattern synonym:

```haskell
x,y :: V [String,Int]
x = V "test"
y = V @Int 10
```

For now the compiler *cannot use the variant value type list to infer the type
of the variant value*!  In the previous example we have to specify the `Int`
type. Even if it's clear (for us) that it's the obvious unique possibility, it
is ambiguous for the compiler.

```haskell
x :: V [String,Int]
x = V 10

-- Test.hs:12:5: error:
--     • Couldn't match type ‘Haskus.Utils.Types.List.IsMember'
--                              [String, Int] c0 [String, Int]’
--                      with ‘'True’
--         arising from a use of ‘V’
--       The type variable ‘c0’ is ambiguous
--     • In the expression: V 10
--       In an equation for ‘x’: x = V 10
```

=== How to pattern-match on variant values? <variant-pm>

Matching a variant value can be done with the `V` pattern synonym too:

```haskell
f :: V [String,Int] -> String
f = \case
   V s            -> "Found string: " ++ s
   V (i :: Int)   -> "Found int: " ++ show i
   _              -> undefined
```

For now the compiler *cannot use the variant value type list to infer that the
pattern-match is complete*. Hence we need the wildcard match to avoid a warning.

In section @variant-safe-pm we present another way to perform pattern
matching that doesn't require a wildcard match and that provides better type
inference.

*Basic errors*

If you try to set or match a value type that isn't valid, you get a compile-time
error:

```haskell
x :: V [String,Int]
x = V @Float 10

-- Test.hs:14:5: error:
--     • `Float' is not a member of [String, Int]
--     • In the expression: V @Float 10
--       In an equation for ‘x’: x = V @Float 10
```

```haskell
f :: V [String,Int] -> String
f = \case
  V s            -> "Found string: " ++ s
  V (i :: Int)   -> "Found int: " ++ show i
  V (i :: Float) -> "Found float: " ++ show i
  _              -> undefined

-- Test.hs:20:4: error:
--     • `Float' is not a member of [String, Int]
--     • In the pattern: V (i :: Float)
--       In a case alternative: V (i :: Float) -> "Found float: " ++ show i
```

==== How to pattern-match on variants safely? <variant-safe-pm>

Let's suppose we have the following variant values that we want to pattern-match
on:

```haskell
x,y,z :: V [String,Int,Float]
x = V "test"
y = V @Int 10
z = V @Float 5.0
```

*Unsafe pattern-matching*

We can use pattern matching on the constructors as presented in section
@variant-pm:

```haskell
printV' :: V [String,Int,Float] -> IO ()
printV' = \case
   V (s :: String) -> putStrLn ("Found string: " ++ s)
   V (i :: Int)    -> putStrLn ("Found int: " ++ show i)
   V (f :: Float)  -> putStrLn ("Found float: " ++ show f)
   _               -> undefined

> printV' x
Found string: test
> printV' y
Found int: 10
> printV' z
Found float: 5.0
```

However the compiler cannot detect that the pattern matching is complete, hence
we have the choice between a warning or adding a wildcard match as we have done
above.

Now we present safer alternatives:

*Safe pattern-matching with continuations (`>:>`)*

Another solution is to rely on multi-continuations. Then we can provide a
function per constructor as in a pattern-matching. For instance, with
multi-continuations we can transform a variant `V [A,B,C]` into a function
whose type is `(A -> r, B -> r, C -> r) -> r`. Hence the compiler will ensure
that we provide the correct number of alternatives in the continuation tuple
(the first parameter).

Applying a multi-continuation to a Variant is done with `>:>`:

```haskell
import Haskus.Utils.ContFlow

printV :: V [String,Int,Float] -> IO ()
printV v = v >:>
   ( \s -> putStrLn ("Found string: " ++ s)
   , \i -> putStrLn ("Found int: " ++ show i)
   , \f -> putStrLn ("Found float: " ++ show f)
   )
```

*Safe pattern-matching with unordered continuations (`>%:>`)*

By using the `>%:>` operator instead of `>:>`, we can provide continuations in
any order as long as an alternative for each constructor is provided.

The types must be unambiguous as the Variant constructor types can't be used to
infer the continuation types (as is done with `>:>`). Hence the type
ascriptions in the following example:

```haskell
printU :: V [String,Int,Float] -> IO ()
printU v = v >%:>
   ( \f -> putStrLn ("Found float: " ++ show (f :: Float))
   , \s -> putStrLn ("Found string: " ++ s)
   , \i -> putStrLn ("Found int: " ++ show (i :: Int))
   )
```

=== How to get/set values by its type index?

We can explicitly create a variant by specifying the index (starting from 0) of
the value type with `toVariantAt`:

```haskell
x :: V [Int,String,Float]
x = toVariantAt @2 5.0
```

It is especially useful if for some reason we want to have the same type more
than once in the variant value type list:

```haskell
y :: V [Int,Int,String,Int,Float]
y = toVariantAt @1 5
```

We can retrieve values by index too with `fromVariantAt`:

```haskell
> fromVariantAt @0 x
Nothing
> fromVariantAt @1 x
Nothing
> fromVariantAt @2 x
Just 5.0
```

=== How to code generic variant functions (variant-polymorphic functions)?

In this section we show how to write generic functions that can work on
different `Variant` as long as they fulfill some constraints.

==== How to split a variant?

We can chose to handle only a subset of the possible value types of a Variant by
using `splitVariant`. This is very useful when your variant is open (e.g. an
exception type) and you want to perform an action for some particular
types while ignoring the others (e.g. passing the unhandled exceptions to the
caller).

For instance in the following example we only handle `Int` and `Float`
values. The other ones are considered as left-overs:

```haskell
printNum v = case splitVariant @[Float,Int] v of
   Right v -> v >%:>
      ( \f -> putStrLn ("Found float: " ++ show (f :: Float))
      , \i -> putStrLn ("Found int: " ++ show (i :: Int))
      )
   Left leftovers -> putStrLn "Not a supported number!"

> printNum x
Not a supported number!
> printNum y
Found int: 10
> printNum z
Found float: 5.0
```

Note that the `printNum` function above is generic and can be applied to any
Variant type:

```haskell
w,k,u :: V [String,Int,Double,Maybe Int]
w = V @Double 1.0
k = V (Just @Int 10)
u = V @Int 17

> printNum w
Not a supported number!
> printNum k
Not a supported number!
> printNum u
Found int: 17
```

==== `:<` and `:<<` operators

The `c :< cs` constraint statically ensures that the type `c` is in the
`cs` type list and that we can set and match it in a variant with type `V
cs`. For example:

```haskell
newtype Error = Error String

showError :: (Error :< cs) => V cs -> String
showError = \case
   V (Error s) -> "Found error: " ++ s
   _           -> "Not an Error!"
```

We check that `showError` works:
  
```haskell
e0,e1 :: V [String,Int,Error]
e0 = V (Error "invalid")
e1 = V @Int 10

> showError e0
"Found error: invalid"

> showError e1
"Not an Error!"
```

The same generic `showError` function works with variants of other types as
well:
  
```haskell
e2 :: V [Float,String,Maybe Char,Error]
e2 = V (Error "Oups!")

e3 :: V [Error]
e3 = V (Error "Outch!")

> showError e2
"Found error: Oups!"

> showError e3
"Found error: Outch!"
```

Note that to shorten a list of constraints such as `(A :< xs, B :< xs, C :< xs)`
you can use the `:<<` operator: `[A,B,C] :<< xs`.

==== `:<?` operator and `VMaybe` pattern

The `c :< cs` constraint statically ensures that the type `c` is in the `cs`
type list. However in some cases we want to write generic functions that work on
variants even if they can't contain the given type.

For instance if we try to apply the `showError` function of the previous
example on a variant that can't contain a value of type `Error`, we get the
following expected compile-time error:

```haskell
e4 :: V [String,Int]
e4 = V "valid"

> showError e4

-- <interactive>:45:1: error:
--     • `Error' is not a member of [String, Int]
```


Nevertheless we can write a `showErrorMaybe` that works on any variant even if
it can't contain an `Error` value by using the `:<?` constraint constructor
and by matching with `VMaybe` as follows:

```haskell
showErrorMaybe :: (Error :<? cs) => V cs -> String
showErrorMaybe = \case
   VMaybe (Error s) -> "Found error: " ++ s
   _                -> "Not an Error!"

> showErrorMaybe e0
"Found error: invalid"

> showErrorMaybe e1
"Not an Error!"

> showErrorMaybe e2
"Found error: Oups!"

> showErrorMaybe e3
"Found error: Outch!"

> showErrorMaybe e4
"Not an Error!"
```

Obviously this example is a bit contrived because we can easily see that `e4`
can't contain an `Error`. However the same `:<?` constraint is also used to
define some more interesting operations as shown below.

==== How to shrink variants with `popVariant`?

A very common use of variants is to pattern match on a specific value type they
can contain and to get a new variant containing the left-over value types. This
is done with `popVariant` or `popVariantMaybe` and the `Remove` type
family. For example:

```haskell
filterError :: Error :<? cs => V cs -> V (Remove Error cs)
filterError v = case popVariantMaybe v of
   Right (Error s) -> error ("Found error: " ++ s)
   Left  v'        -> v' -- left-over variant!


> filterError e0
*** Exception: Found error: invalid
CallStack (from HasCallStack):
  error, called at Test.hs:61:23 in main:Main

> filterError e1
10

> :t e1
e1 :: V [String, Int, Error]

> :t filterError e1
filterError e1 :: V [String, Int]

> :t e2
e2 :: V [Float, String, Maybe Char, Error]

> :t filterError e2
filterError e2 :: V [Float, [Char], Maybe Char]
```

Notice how an `Error` value can't be present anymore in the variant type
returned by `filterError` and how this function is generic as it supports any
variant as an input.

Similarly we could have used the `Error <: cs` constraint and the
`popVariant` function to ensure that only variants that can contain an
`Error` value can be passed to the `filterError` function.

=== How to convert a variant from/to a singleton value

We can easily convert between a variant with a single value type and this value
type with `variantToValue` and `variantFromValue`:

```haskell
intV :: V [Int]
intV = V @Int 10

> variantToValue intV
10

> :t variantToValue intV
variantToValue intV :: Int

> :t variantFromValue "Test"
variantFromValue "Test" :: V [String]
```

`variantFromValue` is especially useful to avoid having to define the value
types of the variant explicitly.

=== How to convert a variant from/to Either?

`variantFromEither` and `variantToEither` can be used to convert between a
variant of arity 2 and the `Either` data type:

```haskell
eith :: Either Int String
eith = Left 10

> :t variantFromEither eith
variantFromEither eith :: V [String, Int]

x,y :: V [String,Int]
x = V "test"
y = V @Int 10

> variantToEither x
Right "test"

> variantToEither y
Left 10
```

=== How to extend the list of supported types of a variant?

We can extend the value types of a variant by appending or prepending a list of
types with `appendVariant` and `prependVariant`:

```haskell
x :: V [String,Int]
x = V "test"

data A = A
data B = B

px = prependVariant @[A,B] x
ax = appendVariant @[A,B] x

> :t ax
ax :: V [String, Int, A, B]

> :t px
px :: V [A, B, String, Int]
```

You can use the `Concat` type family to specify the type of a concatened
variant:

```haskell
data Error0 = Error0 deriving Show
data Error1 = Error1 deriving Show

checkErr ::
   ( Int :< is
   , os ~ Concat is [Error0,Error1]
   , Error0 :< os
   , Error1 :< os
   ) => V is -> V os
checkErr = \case
   V (0 :: Int) -> V Error0
   V (1 :: Int) -> V Error1
   v            -> appendVariant @[Error0,Error1] v

> checkErr (V @Int 0 :: V [Float,Int])
V @Error0 Error0

> checkErr (V @Int 1 :: V [Float,Int])
V @Error1 Error1

> checkErr (V @Int 2 :: V [Float,Int])
V @Int 2

> checkErr (V @Float 5.0 :: V [Float,Int])
V @Float 5.0

> z = checkErr (V @Float 5.0 :: V [Float,Int,String,Double])
> :t z
z :: V [Float, Int, [Char], Double, Error0, Error1]
```

Appending and prepending are very cheap operations: appending just messes with
types and performs nothing at runtime; prepending only increases the tag value
at runtime by a constant number.

These operations are used to convert a variant value into another one with a
more general variant type. See also variant lifting (@variant-lifting) which
does variant appending/prepending automatically as well as type reordering.

=== How to extend and reorder the types of a variant? A.k.a variant lifting <variant-lifting>

We can extend and reorder the value types of a variant with the `liftVariant`
function:

```haskell
x :: V [String,Int]
x = V "test"

-- adding Double and Float, and reordering
y :: V [Double,Int,Float,String]
y = liftVariant x
```

You can use the `LiftVariant is os` constraint to write generic code and to
ensure that the type list `is` is a subset of `os`:

```haskell
liftX :: (LiftVariant is (Double : Float : is))
      => V is -> V (Double : Float : is)
liftX = liftVariant

> :t liftX x
liftX x :: V [Double, Float, String, Int]

> :t liftX (V "test" :: V [String])
liftX (V "test" :: V [String]) :: V [Double, Float, String]
```

=== How to remove duplicates in a variant type list? <variant-nub>

If the list of types of a variant contains the same type more than once, we can
decide to only keep one of them with `nubVariant`:

```haskell
> z = nubVariant (V "test" :: V [String,Int,Double,Float,Double,String])
> :t z
z :: V [String, Int, Double, Float]
```

You can use the `Nub` type family to write generic code.

=== How to flatten nested variants? <variant-flatten>

If the value types of a variant are themselves variants, you can flatten them
with `flattenVariant`:

```haskell
x :: V [String,Int]
x = V "test"

nest :: V [ V [String,Int], V [Float,Double]]
nest = V x

> :t flattenVariant nest
flattenVariant nest :: V [String, Int, Float, Double]
```

You can use the `Flattenable` type-class and the `FlattenVariant` type
family to write generic code.

=== How to join variants of functors/monads?

We can transform a variant of functor values (e.g., `V [m a, m b, m c]`) into
a single functor value (e.g., `m (V [a,b,c])`) with `joinVariant`:

```haskell
fs0,fs1,fs2 :: V [ Maybe Int, Maybe String, Maybe Double]
fs0 = V @(Maybe Int) (Just 10)
fs1 = V (Just "Test")
fs2 = V @(Maybe Double) Nothing

> joinVariant @Maybe fs0
Just (V @Int 10)

> joinVariant @Maybe fs1
Just (V @[Char] "Test")

> joinVariant @Maybe fs2
Nothing
```

It also works with `IO` for example:

```haskell
printRet :: Show a => a -> IO a
printRet a = do
   print a
   return a

ms0,ms1 :: V [ IO Int, IO String, IO Double]
ms0 = V @(IO Int) (printRet 10)
ms1 = V (printRet "Test")

> joinVariant @IO ms0
10
V @Int 10

> joinVariant @IO ms1
"Test"
V @[Char] "Test"

> :t joinVariant @IO ms0
joinVariant @IO ms0 :: IO (V [Int, String, Double])
```

Writing generic code requires the use of the `JoinVariant m xs` constraint and
the resulting list of value types can be obtained with the `ExtractM m xs`
type family.

```haskell
> :t joinVariant
joinVariant :: JoinVariant m xs => V xs -> m (V (ExtractM m xs))
```


With `IO` it is possible to use the `joinVariantUnsafe` function which doesn't
require the type application and doesn't use the `JoinVariant` type-class.
However some other functor types aren't supported (e.g., `Maybe`) and using
`joinVariantUnsafe` with them makes the program crash at runtime.

=== How to combine two variants? (i.e. variant produce)

We can combine two variants into a single variant containing a tuple with
`productVariant`:

```haskell
fl :: V [Float,Double]
fl = V @Float 5.0

d :: V [Int,Word]
d = V @Word 10

dfl = productVariant d fl

> dfl
V @(Word,Float) (10,5.0)

> :t dfl
dfl :: V [(Int, Float), (Int, Double), (Word, Float), (Word, Double)]
```

=== How to convert variants to tuples/HList?

We can convert a Variant into a tuple of Maybes with `variantToTuple`:

```haskell
w,k,u :: V [String,Int,Double,Maybe Int]
w = V @Double 1.0
k = V (Just @Int 10)
u = V @Int 17

> :t variantToTuple w
variantToTuple w :: (Maybe String, Maybe Int, Maybe Double, Maybe (Maybe Int))

> variantToTuple w
(Nothing,Nothing,Just 1.0,Nothing)

> variantToTuple k
(Nothing,Nothing,Nothing,Just (Just 10))

> variantToTuple u
(Nothing,Just 17,Nothing,Nothing)
```

And similarly into an HList (heterogeneous list) with `variantToHList`:

```haskell
> variantToHList w
H[Nothing,Nothing,Just 1.0,Nothing]

> variantToHList k
H[Nothing,Nothing,Nothing,Just (Just 10)]

> variantToHList u
H[Nothing,Just 17,Nothing,Nothing]
```

=== How to map a variant type? <variant-map>

We can easily apply a function `f :: A -> B` to a variant so that its value
type `A` is replaced with `B`. If the value in the variant has type `A`,
then `f` is applied to it to get the new value. Example:

```haskell
x,y :: V [String,Int]
x = V "test"
y = V @Int 10

> mapVariant ((+5) :: Int -> Int) x
V @String "test"

> mapVariant ((+5) :: Int -> Int) y
V @Int 15
```

Note that the resulting variant may contain the same type more than once. To
avoid this, we can either use `nubVariant` (see @variant-nub) or directly use
`mapNubVariant`:

```haskell
> :t mapVariant (length :: String -> Int) x
mapVariant (length :: String -> Int) x :: V [Int, Int]

> :t mapNubVariant (length :: String -> Int) x
mapNubVariant (length :: String -> Int) x :: V [Int]

> mapNubVariant (length :: String -> Int) x
V @Int 4
```

Generic code can be written with the `MapVariant a b cs` constraint and the
`ReplaceAll` type family so that: `mapVariant :: MapVariant a b cs => (a ->
b) -> V cs -> V (ReplaceAll a b cs)`

=== How to map a single variant type by its index? <variant-map-index>

If we know the index of the value type we want to map, we can use
`mapVariantAt`. Example:

```haskell
x,y :: V [String,Int]
x = V "test"
y = V @Int 10

> mapVariantAt @0 length x
V @Int 4

> mapVariantAt @0 length y
V @Int 10

> mapVariantAt @1 (+5) x
V @[Char] "test"

> mapVariantAt @1 (+5) y
V @Int 15
```

Note that the compiler uses the type of the element whose index is given as
first argument to infer the type of the functions `length` and `+5`, hence
we don't need type ascriptions.

We can use `mapVariantAtM` to perform an applicative (or monadic) update. For
example:

```haskell
add :: Int -> Int -> IO Integer
add a b = do
   putStrLn "Converting the result into Integer!"
   return (fromIntegral a + fromIntegral b)

> mapVariantAtM @1 (add 5) x
V @[Char] "test"

> mapVariantAtM @1 (add 5) y
Converting the result into Integer!
V @Integer 15
```

=== How to map a the first-matching variant type? <variant-map-first>

A variant can have the same type more than once in its value type list.
`mapVariant` (@variant-map) updates all the matching types in the list but
sometimes that's not what we want. We can use `mapVariantAt`
(@variant-map-index) if we know the index of the type we want to update. We can
also use `mapVariantFirst` as follows if we want to update only the first
matching type:

```haskell
vv :: V [Int,Int,Int]
vv = toVariantAt @1 5

> r0 = mapVariant (show :: Int -> String) vv
> r1 = mapVariantFirst (show :: Int -> String) vv

> :t r0
r0 :: V [String,String,String]

> :t r1
r1 :: V [String, Int, Int]

> r0
V @[Char] "5"

> r1
V @Int 5
```

We can also apply an applicative (or monadic) function with
`mapVariantFirstM`:

```haskell
printRetShow :: Show a => a -> IO String
printRetShow a = do
   print a
   return (show a)

> r2 = mapVariantFirstM (printRetShow @Int) vv
> r2
V @Int 5

> :t r2
r2 :: IO (V [String, Int, Int])
```

// TODO:
// - foldMapVariantFirst[M]
// - foldMapVariant
// - alterVariant
// - traverseVariant

=== How to use do-notation with variants? <variant-do-notation>

Note: the approach presented here uses the dreadful `RebindableSyntax` GHC extension.
We recommend that you use `VEither` instead (@veither).

We can use `do-notation` with `Variant` as we would with other sum types
such as `Maybe` or `Either`. However, as we can't have a `Monad` instance
for `Variant`, we rely on the `RebindableSyntax` extension to mimic it.

The leftmost type is extracted from the Variant with `>>=` (or `x <-
myVariant` with do-notation syntax). Variant types are concatenated on the
left.

Function `foo` in the following example composes functions returning Variants
by using do-notation:

```haskell
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RebindableSyntax #-}

import Haskus.Data.Variant
import Haskus.Data.Variant.Syntax

import Prelude hiding (head,lookup,(>>=),(>>),return)
import qualified Prelude
import Text.Read

foo :: String -> V [Integer, ParseError, LookupError Char, HeadError]
foo str = do
   c <- head str
   r <- lookup c codeMap
   parse (r ++ tail str)

   where
      codeMap :: [(Char, String)]
      codeMap = [ ('x', "0x")
                , ('d', "")
                ]


data ParseError = ParseError deriving Show

parse :: String -> V [Integer,ParseError]
parse s = case readMaybe s of
   Just i  -> V @Integer i                -- we use the `V` pattern to index
   Nothing -> V ParseError                -- the Variant by type

data HeadError = ListWasEmpty deriving Show

head :: [a] -> V [a,HeadError]
head []    = toVariantAt @1 ListWasEmpty  -- we can't index the Variant by
head (x:_) = toVariantAt @0 x             -- type because `a` is ambiguous,
                                          -- so we do it by index explicitly

data LookupError k = KeyWasNotPresent k deriving Show

lookup :: Eq k => k -> [(k,v)] -> V [v,LookupError k]
lookup k vs = case Prelude.lookup k vs of
   Just v  -> toVariantAt @0 v            -- ditto
   Nothing -> toVariantAt @1 (KeyWasNotPresent k)
```


Test:

```haskell
> foo "d10"
V @Integer 10

> foo "x10"
V @Integer 16

> foo "u10"
V @(LookupError Char) (KeyWasNotPresent 'u')

> foo ""
V @HeadError ListWasEmpty

> foo "d10X"
V @ParseError ParseError
```

=== Biased variants: VEither <veither>

Variants have the following kind of types: `V [W,X,Y,Z]`. This is great when
all the inner types play the same role. However in some cases we want one type
to be the main one and the other ones to be secondaries.

For instance we could have the variant whose type is `V
[Result,ErrorA,ErrorB,ErrorC]` to represent the result of a function. In this
case, the first type is the main one and it would be great to be able to define the
common type-classes (Functor, Monad, etc.) so that we have easy access to it.

`VEither` is a Variant wrapper that does exactly this:

```haskell
newtype VEither es a = VEither (V (a : es))
```

It is isomorphic to the following type: `Either (V es) a`. The difference is
in the runtime representation: ``VEither es a`` has one less indirection than
`Either (V es) a` (it uses only one tag value).

==== How to pattern-match on VEither values? (VRight and VLeft)

`VEither es a` values can be created and matched on with the `VRight` and
`VLeft` patterns (just as if we had the `Either (V es) a` type).

```haskell
>>> VRight True :: VEither [String,Int] Bool
VRight True

>>> VLeft (V "failed" :: V [String,Int]) :: VEither [String,Int] Bool
VLeft "failed"
```

*Common instances*

The main advantage of `VEither es a` over `V (a ': es)` is that we
define instances for common type-classes such as Functor, Applicative, Monad,
Foldable, etc.:

```haskell
> let x = VRight True :: VEither [Int,Float] Bool
> fmap (\b -> if b then "Success" else "Failure") x
VRight "Success"

> let x = VRight True  :: VEither [Int,Float] Bool
> let y = VRight False :: VEither [Int,Float] Bool
> (&&) <$> x <*> y
VRight False

> let x   = VRight True    :: VEither [Int,Float] Bool
> let f v = VRight (not v) :: VEither [Int,Float] Bool
> x >>= f
VRight False

> let x   = VRight True    :: VEither [Int,Float] Bool
> let y   = VLeft (V "failed" :: V [String,Int]) :: VEither [String,Int] Bool
> forM_ x print
True
> forM_ y print
```

== Multi-exceptions monad: Excepts <variant-excepts>

Just like
#link("https://hackage.haskell.org/package/transformers-0.6.1.1/docs/Control-Monad-Trans-Except.html#g:2")[ExceptT
e m a] from the `transformers` package
wraps a `Either e a` value, we can use a `Excepts es m a` newtype to wraps a
`VEither es a` value (cf @veither) and use standard do-notation.

Example:

```haskell
import Haskus.Data.Variant.Excepts

import Prelude hiding (head,lookup)
import qualified Prelude
import Text.Read

data ParseError = ParseError deriving Show

parse :: String -> Excepts [ParseError] IO Integer
parse s = case readMaybe s of
   Just i  -> pure i
   Nothing -> throwE ParseError


data HeadError = ListWasEmpty deriving Show

head :: [a] -> Excepts [HeadError] IO a
head []    = throwE ListWasEmpty
head (x:_) = pure x

data LookupError k = KeyWasNotPresent k deriving Show

lookup :: Eq k => k -> [(k,v)] -> Excepts [LookupError k] IO v
lookup k vs = case Prelude.lookup k vs of
   Just v  -> pure v
   Nothing -> throwE (KeyWasNotPresent k)


   foo :: String -> Excepts [ParseError, LookupError Char, HeadError] IO Integer
   -- foo :: forall es.
   --    ([HeadError,ParseError,LookupError Char] :<< es
   --    ) => String -> Excepts es IO Integer
   foo str = do
      c <- liftE $ head str
      r <- liftE $ lookup c codeMap
      liftE $ parse (r ++ tail str)

      where
         codeMap :: [(Char, String)]
         codeMap = [ ('x', "0x")
                   , ('d', "")
                   ]
```

Test:

```haskell
> runE (foo "d10")
VRight 10

> runE (foo "x10")
VRight 16

> runE (foo "u10")
VLeft KeyWasNotPresent 'u'

> runE (foo "")
VLeft ListWasEmpty

> runE (foo "d10X")
VLeft ParseError

> runE (foo "" `catchE` (\ListWasEmpty -> success 42) :: Excepts [ParseError,LookupError Char] IO Integer)
VRight 42
```


