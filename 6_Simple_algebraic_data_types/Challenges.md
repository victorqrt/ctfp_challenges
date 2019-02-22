# 6.5 Challenges

## 1.
Let `f` be the morphism from `Maybe a` to `Either () a` such that:
- `f: x -> Left ()` if x is `Nothing`
- `f: x -> Right x` if x matches `Just a`

Let `g` be the morphism from `Either () a` to `Maybe a` such that:
- `g: x -> Nothing` if x is `Left ()`
- `g: x -> Just x` if x is `Right a`

We have `f . g = g . f = id`, hence `Maybe a` and `Either () a` are isomorphic.

## 2.
See `algebraic.cc`.

## 3.
To implement `circ` in C++ we need to touch both inheriting classes (and Shape if we want the function to be a requirement), see `algebraic.cc`.

## 4.
If we inherit from `Rect` directly, we have less code to write (and we do not touch anything), but we store the square side length twice in the object.

## 5.
Let `f` be the morphism from `Either a a` (a + a) to `(Bool, a)` (2 * a) such that:
- `f: x -> (True, x)` if x is `Left a`
- `f: x -> (False, x)` if x is `Right a`

Let `g` be the morphism from `(Bool, a)` (2 * a) to `Either a a` (a + a)  such that:
- `g: x -> Left x` if x is `(True, a)`
- `g: x -> Right x` if x is `(False, a)`

We have `f . g = g . f = id`, `Either a a` and `Pair Bool a` are isomorphic. The a + a = 2 * a identity holds for types, up to isomorphism.
