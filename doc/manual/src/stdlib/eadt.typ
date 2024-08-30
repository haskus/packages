
== Extensible recursive ADTs (EADTs) <eadt>

EADTs are "extensible algebraic data types": they can be transformed (by adding
or removing constructors) and their constructors are not tied to a specific EADT
type, hence we can use them as constructors of different EADTs.

EADT constructors and operations can be defined independently (even in different
modules) allowing a great modularity. As such there are an answer to the
"expression problem" (cf @eadt-background section).

.. toctree::
   :maxdepth: 1
   :numbered:

   eadt/safe_pattern_matching
   eadt/constructor_removal
   eadt/constructor_split
   eadt/background

=== Introduction

==== Motivating example

Suppose we want to encode lambda-calculus using an ADT. We could use the
following one:

```haskell
data Expr n -- "n" represents a variable name
   = Lambda n (Expr n)
   | Var n
   | App (Expr n) (Expr n)
```

We can define a pretty-print operation:

```haskell
prettyPrint :: Show n => Expr n -> String
prettyPrint = \case
   Var n      -> show n
   Lambda n e -> mconcat ["\\",show n,".",prettyPrint e]
   App e1 e2  -> mconcat ["(",prettyPrint e1,") (",prettyPrint e2,")"]
```

And we can test on an example:

```haskell
sampleDouble :: Expr String
sampleDouble = Lambda "x" (Var "+" `App` Var "x" `App` Var "x")

> putStrLn (prettyPrint sampleDouble)
\"x".(("+") ("x")) ("x")
```

Now suppose that we want to add support for annotations. We can define a new
expression ADT with an additional constructor:

```haskell
data AExpr a n -- "n" represents a variable name, "a" represents an annotation
   = ALambda n (AExpr a n)
   | AVar n
   | AApp (AExpr a n) (AExpr a n)
   | Ann a (AExpr a n)
```

But now we need to rewrite our operations and expressions (such as "prettyPrint"
and "sampleDouble") to handle and to use the constructors of the new expression
ADT:

```haskell
prettyPrintA :: (Show n, Show a) => AExpr a n -> String
prettyPrintA = \case
   AVar n      -> show n
   ALambda n e -> mconcat ["\\",show n,".",prettyPrintA e]
   AApp e1 e2  -> mconcat ["(",prettyPrintA e1,") (",prettyPrintA e2,")"]
   Ann a e     -> mconcat ["{",show a,"} ", prettyPrintA e]


sampleDoubleA :: AExpr a String
sampleDoubleA = ALambda "x" (AVar "+" `AApp` AVar "x" `AApp` AVar "x")

sampleAnnA :: AExpr String String
sampleAnnA = Ann "Double its input" sampleDouble
```

Now the problem is that we have two totally independent expression types
(`Expr` and `AExpr`) with different operations (`prettyPrint` vs
`prettyPrintA`) which can't be easily mixed. Moreover to define
`prettyPrintA` we had to copy-paste `prettyPrint` just to add a single case
alternative. Now suppose that we want to add a new function (e.g. to compute
free variables of an expression): should we implement it for `Expr`, for
`AExpr`, for both?

Finally suppose that we want to add some other constructors: we either get a
combinatorial explosion of ADTs and functions, or we give up on static checking
and use the "largest" ADT (which contains a superset of the constructors of the
others) with some conventions, e.g. comments and runtime assertions such as "at
this point this expression shouldn't contain any annotation" that are not
enforced by the compiler.

==== Motivating example with EADTs

The same example with EADTs would be written as follows. First we define the
EADTs:

```haskell
import Haskus.Data.Variant.EADT
import Haskus.Data.Variant.EADT.TH

data LambdaF n e = LambdaF n e deriving Functor
data VarF    n e = VarF    n   deriving Functor
data AppF      e = AppF    e e deriving Functor
data AnnF    a e = AnnF    a e deriving Functor

eadtPattern 'LambdaF "Lambda"
eadtPattern 'VarF    "Var"
eadtPattern 'AppF    "App"
eadtPattern 'AnnF    "Ann"

type Expr    n = EADT [LambdaF n, VarF n, AppF]
type AExpr a n = EADT [LambdaF n, VarF n, AppF, AnnF a]
```

Then we define the `prettyPrint` operation by using type classes:

```haskell
class PrettyPrint f where
   prettyPrint' :: f String -> String

instance Show n => PrettyPrint (VarF n) where
   prettyPrint' (VarF n) = show n

instance Show n => PrettyPrint (LambdaF n) where
   prettyPrint' (LambdaF n e) = mconcat ["\\",show n,".",e]

instance PrettyPrint AppF where
   prettyPrint' (AppF e1 e2) = mconcat ["(",e1,") (",e2,")"]

instance Show a => PrettyPrint (AnnF a) where
   prettyPrint' (AnnF a e) = mconcat ["{",show a,"} ",e]

prettyPrint :: BottomUp PrettyPrint xs String => EADT xs -> String
prettyPrint e = bottomUp (toBottomUp @PrettyPrint prettyPrint') e
```

We can test it with:

```haskell
sampleDouble :: Expr String
sampleDouble = Lambda "x" (Var "+" `App` Var "x" `App` Var "x")

sampleAnn :: AExpr String String
sampleAnn = Ann "Double its input" (liftEADT sampleDouble)

> putStrLn (prettyPrint sampleDouble)
\"x".(("+") ("x")) ("x")

> putStrLn (prettyPrint sampleAnn)
{"Double its input"} \"x".(("+") ("x")) ("x")
```

=== EADT basics <eadt-basics>

EADTs can be found in the `haskus-base` package.

You need the following imports in your source:

```haskell
import Haskus.Data.Variant.EADT
import Haskus.Data.Variant.EADT.TH -- template-haskell helpers
```

==== Defining constructors

EADT constructors are data types that must have a `Functor` type-class instance.
Fortunately defining such data types is easy thanks to the `DeriveFunctor`
extension that automatically generates the Functor instance for us.

For instance, let's define the constructors for a list:

```haskell
{-# LANGUAGE DeriveFunctor #-}

data ConsF a e = ConsF a e deriving (Functor)
data NilF    e = NilF      deriving (Functor)
```

Note that *both* data types are parameterised by `e` even if `e` isn't used
in `NilF` definition.

==== Defining pattern synonyms <eadt-pattern-synonyms>

We can match EADT values with the `VF` pattern synonym ("VF" stands for "Variant
Functor"). To make the use of EADTs more pleasant, it is highly recommended to
define an additional pattern synonym for each constructor:

```haskell
pattern Cons :: ConsF a :<: xs => a -> EADT xs -> EADT xs
pattern Cons a l = VF (ConsF a l)

pattern Nil :: NilF :<: xs => EADT xs
pattern Nil = VF NilF
```

These patterns hide the use of the `VF` pattern and make the code much easier
to work with.

As this code is very straightforward to write, we provide Template-Haskell
helpers to generate them automatically. The previous patterns can be generated
with:

```haskell
{-# LANGUAGE TemplateHaskell #-}

import Haskus.Data.Variant.EADT.TH

eadtPattern 'ConsF "Cons"
eadtPattern 'NilF  "Nil"
```

==== Defining the EADT

An EADT is just a type alias as in the following `List` EADT example:

```haskell
type List a = EADT [ConsF a, NilF]
```

==== Creating values

Thanks to the pattern synonyms defined above, we can define values as we would
with a normal ADT:

```haskell
strList :: List String
strList = Cons "How" (Cons "are" (Cons "you?" Nil))
```

In some cases we have to help the type-checker to determine some types. For
instance, in the following example it can't infer the `a` type in `ConsF a`,
hence we have to use type ascriptions:

```haskell
intList :: List Int
intList = Cons (10 :: Int) $ Cons (20 :: Int) $ Cons (30 :: Int) Nil
```

This is because the code is generic enough that the same pattern synonyms could
be used to build an heterogeneous list. For instance containing both `Int` and
`Float`:

```haskell
mixedList :: EADT [ConsF Int, ConsF Float, NilF]
mixedList = Cons (10 :: Int) $ Cons (5.0 :: Float) $ Cons (30 :: Int) Nil
```

We could also easily define another pattern synonym when we work on `List` to
help the inference algorithm:

```haskell
-- pattern for a specific EADT: List a
pattern ConsList :: a -> List a -> List a
pattern ConsList a l = Cons a l
```

We can see that when we use it we don't need type ascriptions because the
`Int` type is propagated:

```haskell
intList :: List Int
intList = ConsList 10 $ ConsList 20 $ ConsList 30 Nil
```

==== Matching values

It is easy and tempting to use the same pattern synonyms to match EADT values.
And indeed this works pretty well:

```haskell
showEADTList :: Show a => List a -> String
showEADTList = \case
   ConsList a l -> show a ++ " : " ++ showEADTList l
   Nil          -> "Nil"
   _            -> undefined

> putStrLn (showEADTList strList)
"How" : "are" : "you?" : Nil

> putStrLn (showEADTList intList)
10 : 20 : 30 : Nil
```

However this approach is a unsatisfactory for two reasons:

   1.  The pattern matching isn't safe: for now the compiler cannot use the
       EADT constructor type list to infer that the pattern-match is
       complete. Hence we need the wildcard match to avoid a warning and to
       use ``ConsList`` to help the type inference. A better alternative is
       presented in the :ref:`safe pattern-matching
       <eadt_safe_pattern_matching>` chapter.

   2. The function isn't generic: if we would like to write a ``showEADTList``
      function that also works on the heterogeneous ``mixedList`` above or on
      any future EADT provided its constructors can be handled, we need to
      use another approach based on type-classes. This is presented in the
      following chapters. 

=== Explicit recursive traversal <eadt-explicit-recursive>

When we need to traverse a data structure, we can either use predefined
traversal functions (e.g., `map`, `fold`, etc.) or write the recursive
function explicitly. EADTs are no different in this regard.

In this chapter we explain how to write explicitly recursive functions for
EADTs: similarly to usual ADTs, it's better to use them only when generic
traversal functions (presented in following chapters) don't fit the bill.

==== Traversal example

If we were to write a `show` function for a list ADT, we could do it like
this:

```haskell
data List a = Cons a (List a) | Nil

showList :: Show a => List a -> String
showList = \case
   Nil      -> "Nil"
   Cons a l -> show a ++ " : " ++ showList l
```

In `showList` we can pattern match on the constructors of `List a` because
the constructor list is closed.  With EADTs the list of constructors isn't
closed and we want to be able to use the same code even with EADTs extended with
more constructors. To support this, we use type-classes to build the equivalent
of the `case` in `showList` above.

Let's define a class `MyShow` that is very much like `Show` and that we will
use to print any EADT value:

```haskell
class MyShow e where
   myShow :: e -> String
```

We can define instances for the `List` constructors defined in a
previous chapter (@eadt-basics):

```haskell
instance MyShow (NilF e) where
   myShow _ = "Nil"

instance (MyShow e, Show a) => MyShow (ConsF a e) where
   myShow (ConsF a l) = show a ++ " : " ++ myShow l
```

Note how each instance corresponds to an alternative in `showList`.


It also requires some additional instances to traverse the `VariantF`
combinator datatype and the `EADT` recursivity handling datatype:

```haskell
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}

instance MyShow (VariantF f (EADT f)) => MyShow (EADT f) where
   {-# INLINE myShow #-}
   myShow (EADT e) = myShow e

instance MyShow (VariantF [] e) where
   {-# INLINE myShow #-}
   myShow = undefined

instance
      ( MyShow (f e)
      , MyShow (VariantF fs e)
      ) => MyShow (VariantF (f ': fs) e)
   where
      {-# INLINE myShow #-}
      myShow v = case popVariantFHead v of
         Right u -> myShow u
         Left  w -> myShow w
```

Note: this boilerplate code (hopefully always very similar and straightforward)
is the main reason you should strive to use predefined recursion schemes instead
of the explicit approach presented here.

Note: the INLINE pragmas are used to ensure that in the generated code we get
the equivalent of the `case` expression in `showList`.

Now we can test it:

```haskell
strList :: List String
strList = Cons "How" (Cons "are" (Cons "you?" Nil))

intList :: List Int
intList = Cons (10 :: Int) $ Cons (20 :: Int) $ Cons (30 :: Int) Nil

mixedList :: EADT [ConsF Int, ConsF Float, NilF]
mixedList = Cons (10 :: Int) $ Cons (5.0 :: Float) $ Cons (30 :: Int) Nil

> putStrLn (myShow strList)
"How" : "are" : "you?" : Nil

> putStrLn (myShow intList)
10 : 20 : 30 : Nil

> putStrLn (myShow mixedList)
10 : 5.0 : 30 : Nil
```

==== Extension example

If we add a new constructor, such as `NodeF` to build binary trees:

```haskell
data NodeF a e = NodeF a e e deriving (Functor)

eadtPattern 'NodeF "Node"
```

We can also add a `MyShow` instance for `NodeF`:

```haskell
instance (MyShow e, Show a) => MyShow (NodeF a e) where
   myShow (NodeF a l1 l2) = show a ++ "\n|- " ++ indent (myShow l1)
                                   ++ "|- " ++ indent (myShow l2)
      where
         indent' []     = []
         indent' (x:xs) = x : fmap ("   "++) xs
         indent = unlines . indent' . lines
```

Now we can show binary trees as well as lists:

```haskell
tree :: EADT [NodeF Int, NilF]
tree = Node (10 :: Int)
         (Node (5 :: Int) Nil Nil)
         (Node (30 :: Int) Nil Nil)

> putStrLn (myShow tree)
10
|- 5
   |- Nil
   |- Nil
|- 30
   |- Nil
   |- Nil
```

We can also mix up trees and lists by using `ConsF` and `NodeF` in the same
EADT:

```haskell
mixedTree :: EADT [NodeF Int, ConsF Int, NilF]
mixedTree = Node (10 :: Int)
         (Cons (5 :: Int) $ Cons (6 :: Int) $ Cons (7 :: Int) Nil)
         (Node (30 :: Int) Nil Nil)

> putStrLn (myShow mixedTree)
10
|- 5 : 6 : 7 : Nil
|- 30
   |- Nil
   |- Nil

-- Note: the code to display trees isn't very clever so don't use it to
-- display list of trees.
```

=== Constraining constructors with `:<:`

The `:<:` type operator is used to ensure that a constructor is present in an
EADT. For example if we consider the following type signature (that will be
developed in the example below):

```haskell
distr :: (AddF :<: f, MulF :<: f) => EADT f -> Maybe (EADT f)
```

The constructors of `EADT f` are not specified but the constraints `(AddF :<: f,
MulF :<: f)` ensure that at least `AddF` and `MulF` constructors are present.

Note that to shorten a list of constraints such as `(AddF :<: f, MulF :<: f)`
you can use the `:<<:` operator: `[AddF,MulF] :<<: f`.

==== Transformation example

Suppose we have the following EADT for arithmetic expressions:

```haskell
{-# LANGUAGE DeriveFunctor #-}

data ValF e = ValF Int deriving (Functor)
data AddF e = AddF e e deriving (Functor)
data MulF e = MulF e e deriving (Functor)

eadtPattern 'ValF "Val"
eadtPattern 'AddF "Add"
eadtPattern 'MulF "Mul"

type Expr = EADT [ValF, AddF, MulF]
```

We can define some value:

```haskell
e1 :: Expr
e1 = Add (Val 10)
         (Mul (Add (Val 5)
                   (Val 10))
              (Val 7))
```

We can define instances of the `MyShow` class (defined in
@eadt-explicit-recursive):

```haskell
instance MyShow (ValF e) where
  myShow (ValF e) = show e

instance MyShow e => MyShow (AddF e) where
  myShow (AddF x y) = "(" ++ myShow x ++ " + " ++ myShow y ++ ")"

instance MyShow e => MyShow (MulF e) where
  myShow (MulF x y) = "(" ++ myShow x ++ " * " ++ myShow y ++ ")"

> putStrLn (myShow e1)
(10 + ((5 + 10) * 7))
```

Now we can define a transformation that distributes multiplication over
addition as follows:

```haskell
-- distribute multiplication over addition if it matches
distr :: (AddF :<: f, MulF :<: f) => EADT f -> Maybe (EADT f)
distr (Mul a (Add c d)) = Just (Add (Mul a c) (Mul a d))
distr (Mul (Add c d) a) = Just (Add (Mul c a) (Mul d a))
distr _                 = Nothing
```

Note that this function works on any EADT as long as it has `AddF` and
`MulF` constructors. We indicate such constraints with the `:<:` type
operator.

Then we need a helper function that performs the traversal of the EADT:

```haskell
import Control.Arrow ((>>>))

-- bottom up traversal that performs an additional bottom up traversal in
-- the transformed sub-tree when a transformation occurs. 
bottomUpFixed :: Functor (VariantF cs) => (EADT cs -> Maybe (EADT cs)) -> EADT cs -> EADT cs
bottomUpFixed f = project >>> fmap (bottomUpFixed f) >>> embed >>> f'
   where
      f' u = case f u of
         Nothing -> u
         Just v  -> bottomUpFixed f v

-- | Distribute multiplication over addition
distribute :: ([AddF,MulF] :<<: cs) => EADT cs -> EADT cs
distribute = bottomUpFixed distr
```

Note: `bottomUpFixed` is a generic recursion scheme over an EADT. You can read
more on this approach in the dedicated chapter @eadt-recursion-schemes.

Finally we can test the transformation on an example:

```haskell
> putStrLn (myShow e1)
(10 + ((5 + 10) * 7))

> putStrLn (myShow (distribute e1))
(10 + ((5 * 7) + (10 * 7)))
```

==== Extensibility

Suppose we add a `Pow` (power) constructor:

```haskell
data PowF e = PowF e e deriving (Functor)

eadtPattern 'PowF "Pow"

instance MyShow e => MyShow (PowF e) where
  myShow (PowF x y) = "(" ++ myShow x ++ " ^ " ++ myShow y ++ ")"
```

We can now write expressions that use the `Pow` constructor:

```haskell
type Expr2 = EADT [ValF, AddF, MulF, PowF]

e2 :: Expr2
e2 = Pow (Val 10)
         (Mul (Add (Pow (Val 5) (Val 8))
                   (Val 10))
              (Val 7))
```

We can check that our distribution function still works on this new type of
expression without being modified at all:

```haskell
> putStrLn (myShow (distribute e2))
(10 ^ (((5 ^ 8) * 7) + (10 * 7)))
```

=== Recursion schemes and EADTs <eadt-recursion-schemes>

Traversing an EADT explicitly (see @eadt-explicit-recursive) can be
tedious. Another approach consists in using dedicated composable combinators
called *recursion schemes*.

The well known `map` and `fold` functions are examples of recursion schemes
for lists: these functions handle the recursive traversal of the data structure
and are parameterized by the functions performing the actual work.  Recursion
schemes are a generalization of this approach.

The best introduction to recursion schemes I've read can be found here:
https://blog.sumtypeofway.com/an-introduction-to-recursion-schemes/

To avoid paraphrasing, I recommend that you read it before continuing.

See also: https://bartoszmilewski.com/2013/06/10/understanding-f-algebras/

==== Catamorphism: Show example

Suppose we rewrite our `Show` class like this:

```haskell
class FunctorShow (f :: * -> *) where
  functorShow :: f String -> String
```

We can define instances for `NilF` and `ConsF`:

```haskell
instance FunctorShow NilF where
  functorShow _ = "Nil"

instance (Show a) => FunctorShow (ConsF a) where
  functorShow (ConsF a l) = show a ++ " : " ++ l
```

Note that there is no recursive call in the definition of `ConsF`'s instance:
it is because we are going to use a recursion scheme that will handle the
recursion.

We also need an instance to handle the generic `VariantF` type:

```haskell
instance (AlgVariantF FunctorShow String xs) => FunctorShow (VariantF xs) where
  functorShow = algVariantF @FunctorShow functorShow
```

Finally we can define a generic `eadtShow` function that uses the catamorphism
recursion scheme with the `functorShow` class method.

```haskell
eadtShow :: 
  ( Functor (VariantF xs)
  , FunctorShow (VariantF xs)
  ) => EADT xs -> String
eadtShow = cata functorShow
```

We can test it:

```haskell
intList :: List Int
intList = Cons (10 :: Int) $ Cons (20 :: Int) $ Cons (30 :: Int) Nil

mixedList :: EADT [ConsF Int, ConsF Float, ConsF String, NilF]
mixedList = Cons @Int 10 $ Cons @Float 5.0 $ Cons "Test" Nil

> putStrLn $ eadtShow intList
10 : 20 : 30 : Nil

> putStrLn $ eadtShow mixedList
10 : 5.0 : "Test" : Nil
```


==== Catamorphism: List (a -> a) mapping example

Similarily to the example above, suppose that we want to implement mapping over
an EADT list. We can use the following type-class:

```haskell
class MapEADT a xs (f :: * -> *) where
  -- map the outer constructor of an EADT
  mapEADT1 :: (a -> a) -> f (EADT xs) -> EADT xs
```

We need some instances to handle our EADT constructors:

```haskell
instance (NilF :<: xs) => MapEADT a xs NilF where
  mapEADT1 _ NilF = Nil

instance (ConsF a :<: xs) => MapEADT a xs (ConsF a) where
  mapEADT1 f (ConsF a x) = Cons (f a) x
```

And a additional instance to traverse the `VariantF` combinator datatype:

```haskell
instance (AlgEADT (MapEADT a xs) xs) => MapEADT a xs (VariantF xs) where
  mapEADT1 f = algVariantF @(MapEADT a xs) (mapEADT1 f)
```

Now we can define the `mapEADT` function by using the catamorphism combinator:

```haskell
-- recursively map an EADT
mapEADT :: ( Functor (VariantF xs)
           , MapEADT a xs (VariantF xs)
           ) => (a -> a) -> EADT xs -> EADT xs
mapEADT f = cata (mapEADT1 f)
```

We can test it:

```haskell
intList :: List Int
intList = Cons (10 :: Int) $ Cons (20 :: Int) $ Cons (30 :: Int) Nil

> putStrLn $ eadtShow $ mapEADT ((+5) :: Int -> Int) intList
15 : 25 : 35 : Nil
```

==== Catamorphism: List (a -> b) mapping example

Similarily, we can also support mapping with a function that changes the EADT
type as follow:

```haskell
class TransEADT a b xs xs' (f :: * -> *) where
  transEADT1 :: (a -> b) -> f (EADT xs) -> EADT xs'

instance (NilF :<: xs') => TransEADT a b xs xs' NilF where
  transEADT1 _ NilF = Nil

instance (ConsF b :<: xs', xs ~ xs') => TransEADT a b xs xs' (ConsF a) where
  transEADT1 f (ConsF a x) = Cons (f a) x

instance TransEADT a b xs xs' (VariantF []) where
  transEADT1 _ _ = undefined

instance
   ( TransEADT a b xs xs' f
   , TransEADT a b xs xs' (VariantF fs)
   ) => TransEADT a b xs xs' (VariantF (f : fs)) where
  transEADT1 f v =  case popVariantFHead v of
         Right u -> transEADT1 f u
         Left  w -> transEADT1 f w

transEADT :: ( Functor (VariantF xs)
             , TransEADT a b xs' xs' (VariantF xs)
             ) => (a -> b) -> EADT xs -> EADT xs'
transEADT f = cata (transEADT1 f)
```


Note that we need to specify the resulting type as it can be anything fulfilling
the constraints:

```haskell
> putStrLn $ eadtShow $ (transEADT (fromIntegral :: Int -> Float) intList :: List Float)
10.0 : 20.0 : 30.0 : Nil
```

=== Safe pattern matching with `>:>` <eadt-safe-pm>

Suppose we have the following `List` EADT:

```haskell
data ConsF a l = ConsF a l deriving (Functor)
data NilF    l = NilF      deriving (Functor)

eadtPattern 'ConsF "Cons"
eadtPattern 'NilF  "Nil"

type List a = EADT [ConsF a, NilF]

-- pattern for a specific EADT: List a
pattern ConsList :: a -> List a -> List a
pattern ConsList a l = Cons a l
```

Using classic pattern matching on `List` constructors as we do below isn't
really typesafe because the compiler cannot detect that the pattern matching is
complete, hence we have the choice between a warning or adding a wildcard match:

```haskell
showEADTList :: Show a => List a -> String
showEADTList = \case
   ConsList a l -> show a ++ " : " ++ showEADTList l
   Nil          -> "Nil"
   _            -> undefined -- this line avoids the warning but is unsafe
                             -- if we add constructors in the future
```


A safe alternative is to rely on multi-continuations: we can transform any
`EADT [A,B,C]` into a function whose type is `(A -> r, B -> r, C -> r) ->
r` with the `(>:>)` operator. Then we can safely provide a function per
constructor as in a pattern-matching.


==== xplicit recursion example

```haskell
import Haskus.Utils.ContFlow

showCont' l = l >:>
   ( \(ConsF a r) -> show a ++ " : " ++ showCont' r -- explicit recursion
   , \NilF        -> "Nil"
   )

> showCont' intList
"10 : 20 : 30 : Nil"
```

==== Recursion schemes (e.g. catamorphism)

```haskell
showCont l = l >:>
   ( \(ConsF a r) -> show a ++ " : " ++ r -- no explicit recursion
   , \NilF        -> "Nil"
   )

> cata showCont intList
"10 : 20 : 30 : Nil"
```

See recursion schemes for EADTs (@eadt-recursion-schemes).

=== EADT Constructor removal/transformation <eadt-constructor-removal>

Removing constructors from an EADT is equivalent to transforming every instance
of these constructors into other constructors of another EADT.

We consider 3 cases:

1. Fixed input EADT type; fixed list of constructors to act on

2. Generic input EADT type; fixed list of constructors to act on

3. Generic input EADT type; extensible list of constructors to act on

Note in the 3 cases we need to specify the resulting EADT type as it could be
anything fulfilling the constraints.

==== Fixed input, fixed matches

If the type of the input EADT is fixed, we can use safe pattern-matching
(@eadt-safe-pm) as follows:

```haskell
-- replace Even and Odd constructors with a Cons constructor
removeOddEven l = l >:>
   (\(EvenF a r) -> Cons a r
   ,\(OddF  a r) -> Cons a r
   ,\NilF        -> Nil
   )

eo :: EADT [EvenF Int, OddF Int, NilF]
eo = Even (10 :: Int) $ Odd (5 :: Int) $ Odd (7 :: Int) Nil

> eadtShow (cata removeOddEven eo :: List Int)
"10 : 5 : 7 : Nil"
```

Note that `removeOddEven` only works on a specific EADT. If we want it to work on
any EADT that contains `Even` and `Odd` constructors, read the following
sections.

==== Generic input, fixed matches

If we want `removeEvenOdd` to work on input EADTs of any type, we can extract
the constructors that we are interested in with `splitVariantF` and lift the
left-over constructors with `liftVariantF` as follows:

```haskell
removeOddEven x = case splitVariantF @[EvenF Int, OddF Int] x of
   -- replace Even and Odd constructors with a Cons constructor
   Right v        -> v >:>
                        ( \(EvenF a l) -> Cons a l
                        , \(OddF a l)  -> Cons a l
                        )
   -- do nothing to the other constructors
   Left leftovers -> EADT (liftVariantF leftovers)

eo1 :: EADT [EvenF Int, OddF Int, NilF]
eo1 = Even (10 :: Int) $ Odd (5 :: Int) $ Odd (7 :: Int) Nil

> eadtShow (cata removeOddEven eo1 :: List Int)
"10 : 5 : 7 : Nil"

-- additional `ConsF Int` constructor
eo2 :: EADT [ConsF Int, EvenF Int, OddF Int, NilF]
eo2 = Even (10 :: Int) $ Cons (5 :: Int) $ Odd (7 :: Int) Nil

> eadtShow (cata removeOddEven eo2 :: List Int)
"10 : 5 : 7 : Nil"
```

==== Generic input, extensible matches

If we want the `removeOddEven` pattern match to be extensible, we can use
type-classes with an overlappable instance handling the generic case (i.e. that
only transfers constructors from one EADT to another without modifying them).

```haskell
class RemoveOddEven ys (f :: * -> *) where
   removeOddEven :: f (EADT ys) -> EADT ys

-- replace Odd and Even with Cons
instance ConsF a :<: ys => RemoveOddEven ys (OddF a) where
   removeOddEven (OddF a l) = Cons a l 

instance ConsF a :<: ys => RemoveOddEven ys (EvenF a) where
   removeOddEven (EvenF a l) = Cons a l 

-- handle the combinator
instance
   ( AlgVariantF (RemoveOddEven ys) (EADT ys) xs
   ) => RemoveOddEven ys (VariantF xs)
   where
      removeOddEven = algVariantF @(RemoveOddEven ys) removeOddEven

-- handle remaining constructors
instance {-# OVERLAPPABLE #-} f :<: ys => RemoveOddEven ys f where
   removeOddEven = VF -- keep the other constructors unmodified
```

Test:

```haskell
eo :: EADT [EvenF Int, OddF Int, NilF]
eo = Even (10 :: Int) $ Odd (5 :: Int) $ Odd (7 :: Int) Nil

> eadtShow (cata removeOddEven eo :: List Int)
"10 : 5 : 7 : Nil"

-- EADT with an additional `ConsF Int` constructor
eo2 :: EADT [ConsF Int, EvenF Int, OddF Int, NilF]
eo2 = Even (10 :: Int) $ Odd (5 :: Int) $ Cons (7 :: Int) $ Odd (7 :: Int) Nil

> eadtShow (cata removeOddEven eo2 :: List Int)
"10 : 5 : 7 : 7 : Nil"

-- EADT with an additional `ConsF String` constructor
eo3 :: EADT [ConsF Int, EvenF Int, OddF Int, ConsF String, NilF]
eo3 = Even (10 :: Int) $ Cons "Test" $ Odd (5 :: Int) $ Cons (7 :: Int) $ Odd (7 :: Int) Nil

> eadtShow (cata removeOddEven eo3 :: EADT [ConsF Int, ConsF String, NilF])
"10 : \"Test\" : 5 : 7 : 7 : Nil"
```

We can extend `removeOddEven` to support other constructors by adding new
instances of `RemoveOddEven` for them.

=== Splitting EADT constructors

We can chose to handle only a subset of the constructors of an EADT by using
`splitVariantF`.

For instance in the following example we only handle `EvenF Int` and `OddF Int`
constructors. The other ones are considered as left-overs:

```haskell
alg x = case splitVariantF @[EvenF Int, OddF Int] x of
  Right v        -> v >:>
                       ( \(EvenF a l) -> "Even : " ++ l
                       , \(OddF a l)  -> "Odd : " ++ l
                       )
  Left leftovers -> "something else"
```

We can test this code with:

```haskell
eo :: EADT [EvenF Int, OddF Int, NilF]
eo = cata evenOdd intList'

eo2 :: EADT [ConsF Int, EvenF Int, OddF Int, NilF]
eo2 = Even (10 :: Int) $ Odd (5 :: Int) $ Cons (7 :: Int) $ Odd (7 :: Int) Nil

> cata alg eo
"Odd : Even : Odd : something else"

> cata alg eo2
"Even : Odd : something else"
```

Note that the traversal ends when it encounters an unhandled constructor.


=== Background on EADTs <eadt-background>

==== Why not Variant?

Extensible ADT (EADT) adds support for recursive datatypes to the Variant type
(@variant). Indeed if we tried to define a recursive datatype
(e.g., a list) by using Variants, we would get the following error:

```haskell
data Cons a l = Cons a l
data Nil      = Nil

> type L a = V [Cons a (L a), Nil]

<interactive>:19:2: error:
    Cycle in type synonym declarations:
      <interactive>:19:2-34: type L a = V [Cons a (L a), Nil]
```

The issue is that there is a cyclic definition and it isn't allowed. We could
introduce ad-hoc datatypes (e.g., `newtype L a = L (V [Cons a (L a),Nil])`) to
break this cycle but this would defeat our purpose because the datatype wouldn't
be generic anymore.

`EADT` is the datatype we use to break these cycles. By always using the same
datatype, we can provide functions that work for every EADTs. `EADT` is very
similar to the `Fix` datatype (fixed point of a functor). We use our own type to
declare our own instances.

For example with EADTs we just have to write the following code to declare a
`List`:

```haskell
data ConsF a l = ConsF a l deriving (Functor)
data NilF    l = NilF      deriving (Functor)

type List a = EADT [ConsF a, NilF]
```


==== History

===== The expression problem (1998)

In 1998, Philip Wadler defined the *Expression Problem* as follow:

>  The Expression Problem is a new name for an old problem. The goal is
>  to define a datatype by cases, where one can add new cases to the
>  datatype and new functions over the datatype, without recompiling
>  existing code, and while retaining static type safety

See:
- https://en.wikipedia.org/wiki/Expression_problem
- http://homepages.inf.ed.ac.uk/wadler/papers/expression/expression.txt

In Haskell it is straightforward to add new functions over an ADT. Suppose
we have the following arithmetic expression ADT:

```haskell
data Expr = Val Int | Add Expr Expr
```

We can independently add an evaluator function, potentially in another module:

```haskell
eval :: Expr -> Int
eval (Val x)   =  x
eval (Add x y) = eval x + eval y
```

However if we want to add a new constructor to the ADT (say support for
multiplication), we have to modify both the ADT definition and the functions
using it:

```haskell
data Expr = .... | Mul Expr Expr

eval :: Expr -> Int
....
eval (Mul x y) = eval x * eval y
```

What we want is to be able to add a new independent module containing both the
`Mul` constructor and the code to handle it, without modifying the other
modules defining the other constructors and the other code to handle them!

===== Data types à la carte (2008)

Ten years later (in 2008), Wouter Swierstra described a technique to handle this
in his well-known
#link("http://www.cs.ru.nl/~W.Swierstra/Publications/DataTypesALaCarte.pdf")[Data
types à la carte] paper. The first idea is to define data constructors
independently of the ADT and to use a type parameter to leave open the ADT they
are part of.

```haskell
-- Independent data constructors. Parameter `e` represents the ADT they
-- will be part of. It is required even if it is not used in the right hand
-- side.
data Val e = Val Int deriving (Functor)
data Add e = Add e e deriving (Functor)
```

Defining a new independent constructor is easy:

```haskell
data Mul e = Mul e e deriving (Functor)
```

The second idea is to use a combinator data type `:+:`:

```haskell
data (f :+: g) e = Inl (f e) | Inr (g e)

instance (Functor f, Functor g) => Functor (f :+: g) where ...
```

It is similar to `Either` except that it passes the same additional type
parameter to both `f` and `g` type constructors. It can be used to compose
independent data constructors without creating a new data type:

```haskell
type ExprF = Val :+: Add
```

`ExprF` has kind `Type -> Type` and its type parameter is used as the `e`
parameter of the independent data constructors. We can set it to arbitrary types
such as `Int` to build valid values:

```haskell
y = Inr (Add 5 8) :: ExprF Int
```

However the main use of this parameter should be to indicate the type of the
expression data type we want to build, say `Expr`. Hence we would like to
write something like this:

```haskell
type Expr = ExprF Expr

 >error:
 Cycle in type synonym declarations:
   <interactive>:12:1-22: type Expr = ExprF Expr
```

Oops, we can't build this cyclic (infinite) type. This leads us to the third
idea: use another data type to handle the recursive nature of the expression
type:

```haskell
newtype Expr = Expr (ExprF Expr)
```

We can abstract over it to use the same data type for different expression types:

```haskell
-- `Fix` type as defined in Data.Functor.Foldable for instance
newtype Fix f = Fix (f (Fix f))

type Expr = Fix ExprF
```

In summary, the approach uses 3 different sorts of data types:

1. Constructor data types: `Val`, `Add`, `Mul`...

2. Combinator data type: `:+:`

3. Recursion handling data type: `Fix`

By using these different data types we have untangled the construction of ADTs
(algebraic data types) and we can freely add new constructor data types and mix
them into different algebraic data types.

Operations on these algebraic data types can be defined independently by using
type-classes and recursion schemes.

==== EADT - Extensible ADT (2018)

The EADT approach builds on the Swierstra's one but it replaces the combinator
data type `:+:` with the `VariantF` one based on Variant (@variant).
Similarly to the `:+:` combinator data type, `VariantF` passes its `e`
parameter to all of its "member" types and has an instance of the `Functor`
class.

```haskell
newtype VariantF (xs :: [* -> *]) e = VariantF (Variant (ApplyAll e xs))

-- ApplyAll e [f,g,h] ==> [f e, g e, h e]

instance Functor (VariantF xs) where ....
```

Now instead of writing `f :+: g :+: h :+: i` to combine constructor data types
to form an ADT we can write `VariantF [f,g,h,i]`.  Just like using
`Variant` is more efficient -- O(1) memory usage and (de)construction -- than
using a nest of `Either`, using `VariantF` is more efficient than using a
nest of `:+:`.

Finally an EADT is just `Fix (VariantF xs)` except that we use our own `EADT`
newtype instead of `Fix` in order to define our own additional (and non-orphan)
type-class instances. `EADT` implements `Recursive` and `CoRecursive`
type-classes from the `recursion-schemes` package, so usual `Fix` functions
should work on `EADT` too.

```haskell
newtype EADT xs = EADT (VariantF xs)
```


The next step is to define bidirectional pattern synonyms
(@eadt-pattern-synonyms) that make the manipulation of EADT values very similar
to the manipulation of usual ADTs. By using Template Haskell, these patterns can
be automatically generated.

In summary EADTs provide a nicer interface and a better asymptotic
implementation in both memory and runtime execution than Data types à la carte.
In the future it would be better to have native support for all of this in the
language, especially to enhance compilation times by not using type families.
