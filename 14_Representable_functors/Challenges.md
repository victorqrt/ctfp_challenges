# 14.3 Challenges

## 1.
Functor laws state that when functors lift morphisms from a category C1 to another category C2, they preserve composition and identity. The hom-functor from any category C to Set maps any
identity morhpism in C to the identity function in Set.

## 2.
Showing that a functor F is representable is equivalent to providing an invertible natural transformation between F and the hom-functor. That is, two naural transformations `alpha` and
`beta` such that, for any given a in a category C:

```Haskell
{-# LANGUAGE ExplicitForAll #-}

alpha :: forall x. (a -> x) -> F x -- The hom-functor is Reader

beta :: forall x. F x -> (a -> x)

alpha . beta = beta . alpha = id
```

Let us show that, if F is the `Maybe` functor, such a pair or morphisms cannot exist.
While choosing `alpha` is pretty straightforward ( `alpha f a = Just (f a)` ), defining `beta` requires us to provide a morphism from `Maybe x` to `(a -> x)`. Mapping `Nothing` to a morphism
from a to x implies creating an x out of thin air. The `Maybe` functor is not representable.

## 3.
The Reader functor is by definition equivalent to the hom-functor: it represents morphisms from an object to another in a given category. Therefore, it is representable (the isomorphic
natural transformation is identity).

## 4.
```Haskell
-- Assuming we have

data Stream a = Cons a (Stream a)

class Representable f where
    type Rep f :: *
    tabulate :: (Rep f -> x) -> f x
    index :: f x -> Rep f -> x

instance Representable Stream where
    type Rep Stream = Integer
    tabulate f = Cons (f 0) (tabulate (f . (+1)))
    index (Cons b bs) n = if n == 0 then b else index bs (n-1)

-- The we can memoize f into a Stream of integers

f :: Int -> Int
f x = x * x

memoizedF = tabulate f
```

`index memoizedF sq` where sq is the square of an integer n will yield n from the Stream, memoizing values up to sq on the fly.

## 5.
```Haskell
s :: Stream b
s = Cons b bs

tabulate (index s) = Cons (index s 0) (tabulate ((index s) . (+1)))
```

We see by pattern matching the obtained stream that its head `index s 0` is the head of our stream s. In summary, for every stream, `tabulate . index` is a morphism preseving its head,
the b in `Cons b bs`. In this form `bs` is itself a stream. By induction, `tabulate . index` preserves the stream for any stream: it is the identity morphism. `tabulate` and `index` are inverse of each other.

## 6.
```Haskell
instance Representable Pair where
    type Rep Pair = Bool
    tabulate f = Pair (f True) (f False)
    index (Pair a1 a2) i = if i then a1 else a2
```
