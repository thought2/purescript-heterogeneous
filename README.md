# purescript-heterogenous

[![Latest release](http://img.shields.io/github/release/natefaubion/purescript-heterogenous.svg)](https://github.com/natefaubion/purescript-heterogenous/releases)
[![Build status](https://travis-ci.org/natefaubion/purescript-heterogenous.svg?branch=master)](https://travis-ci.org/natefaubion/purescript-heterogenous)

Maps and folds for heterogenous data types.

## Why?

PureScript has a very rich type-system which lets us explicitly describe the
shape of our data in fine detail. For example, Records and row-types let us
write ergonomic, polymorphic, structurally-typed data. However, writing generic
operations over such data types often involves a lot of tedious, constraint-level
tricks. This library provides a framework which separates the traversal and
summary logic for heterogenous data types, like `Functor` and `Foldable` do for
homogenous types, so we can reduce the boilerplate and tricks while sharing
implementations.

## How to use this library

This library exports several classes for both indexed and unindexed folds and maps.

* `class HMap` and `hmap`
* `class HMapWithIndex` and `hmapWithIndex`
* `class HFoldl` and `hfoldl`
* `class HFoldlWithIndex` and `hfoldlWithIndex`

These are similar to their homogenous counterparts. In fact, the folds and maps we
write for heterogenous types can be reused for homogenous data via the `App f a`
newtype. That is, the following are identical:

```purescript
map f [1, 2, 3]
```
```purescript
hmap f (App [1, 2, 3])
```

Normal functions aren't enough for a lot of the things we need to do though, so
dispatching these is slightly different as we'll need to use PureScript's constraint
system.

The following examples will operate over `Record` types, since it is the most
ubiquitous heterogenous data type in PureScript.

### Example: Mapping over a homogenous Record

Records aren't every actually homogenous to the type system, but sometimes all the
values end up being the same time. Mapping over an apparently homogenous `Record`
is as simple as using a normal (monomorphic) function.

```purescript
hmap (add 1 >>> show) { a: 1, b: 2, c: 3 }
```
```
{ a: "2", b: "3", c: "4" }
```

### Example: Mapping over a heterogenous Record

In the previous example, we can generalize our mapping function:

```purescript
addOneAndShow :: forall n. Semiring n => Show n => n -> String
addOneAndShow = add one >>> show
```

However, we won't be able to dispatch this without giving it a monomorphic type
signature which fixes it to a homogenous `Record` like before. We want to be able to
instantiate these dictionaries for each member individually. To do that, we first
need a data type to represent our mapping function:

```purescript
data AddOneAndShow = AddOneAndShow
```

With this we can define an instance of `Mapping`, which we can use with `hmap`.

```purescript
instance addOneAndShow ::
  (Semiring n, Show n) =>
  Mapping AddOneAndShow n String where
  mapping AddOneAndShow = add one >>> show
```

Then instead of passing a function to `hmap`, we pass in our data type.

```purescript
hmap AddOneAndShow { a: 1, b: 2.0, c: { x: 12, y: 42 } }
```
```
{ a: "2", b: "3.0", c: "{ x: 13, y: 43 }" }
```

### Example: Mapping over a heterogenous Record with additional arguments

With normal functions, we can use partial application to thread in additional
context for our mapping function. We can do the same here by adding arguments
to our mapping data type.

Say we want to implement a `Record` zip operation. That is, we want to map over
a record, and for each field we want to look up a corresponding function in
_another_ record and apply it to the value.

First we need to define a data type for our zip mapping, but we want it to hold
the record of functions:

```purescript
newtype ZipProps fns = ZipProps { | fns }
```

If we use `MappingWithIndex` instead of `Mapping` we can utilize the field name
as well via `SProxy`.

```purescript
instance zipProps ::
  (IsSymbol sym, Row.Cons sym (a -> b) x fns) =>
  MappingWithIndex (ZipProps fns) (SProxy sym) a b where
  mappingWithIndex (ZipProps fns) prop = Record.get prop fns
```

```purescript
let
  zipRecord = hmapWithIndex <<< ZipProps
in
  { a: add 1
  , b: Tuple "bar"
  , c: \a -> not a
  }
  `zipRecord`
  { a: 12
  , b: 42.0
  , c: true
  }
```
```
{ a: 13, b: (Tuple "bar" 42.0), c: false }
```

### Example: Folding over a homogenous Record

Much like with `hmap`, we can fold over homogenous records using normal functions.

```purescript
hfoldl (add :: Int -> Int -> Int) 0 { a: 12, b: 42, c: 100 }
```
```
154
```

The homogenous case needs a monomorphic function, so we've specialized the type of
`add` with a type signature.

### Example: Folding over a heterogenous Record

In this example we will implement an alternative `Show` instance using String
concatenation instead of building an intermediate data structure. First, the
data type for our fold function:

```purescript
data ShowProps = ShowProps
```

We need to follow the same steps as we did with `hmap` and `hmapWithIndex`,
except we will implement `Folding` or `FoldingWithIndex`.

```purescript
instance showProps ::
  (Show a, IsSymbol sym) =>
  FoldingWithIndex ShowProps (SProxy sym) String a String where
  foldingWithIndex ShowProps prop str a =
    pre <> reflectSymbol prop <> ": " <> show a
    where
    pre | str == "" = ""
        | otherwise = str <> ", "
```

Then we can write a wrapper, which adds the curly-braces:

```purescript
showRecord :: forall r.
  HFoldlWithIndex ShowProps String { | r } String =>
  { | r } ->
  String
showRecord r =
  "{ " <> hfoldlWithIndex ShowProps "" r <> " }"
```

```purescript
showRecord { a: "foo" , b: 42 , c: false }
```
```
"{ a: \"foo\", b: 42, c: false }"
```

## Documentation

- Module documentation is [published on Pursuit](http://pursuit.purescript.org/packages/purescript-heterogenous).
